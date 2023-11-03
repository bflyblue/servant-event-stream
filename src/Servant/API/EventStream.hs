{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Servant.API.EventStream
  ( ServerSentEvents
  , EventStream
  , EventSource
  , EventSourceHdr
  , eventSource
  , jsForAPI

  -- * General utilities
  , renderServerEvent
  , builderToText

  -- * Parsing event streams
  , serverEventParser
  )
where

import           Control.Applicative
import           Control.Lens
import qualified Data.Attoparsec.ByteString     as A
import qualified Data.Attoparsec.ByteString.Char8 as A8
import           Data.Binary.Builder            ( toLazyByteString )
import qualified Data.Binary.Builder            as B
import qualified Data.ByteString.Char8          as C8
import qualified Data.ByteString.Lazy.Char8     as BSL8
#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup
#endif
import           Data.Text                      ( Text )
import           Data.Word
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )
import           Network.HTTP.Media             ( (//)
                                                , (/:)
                                                )
import           Network.Wai.EventSource        ( ServerEvent(..) )
import           Network.Wai.EventSource.EventStream
                                                ( eventToBuilder )
import qualified Pipes
import           Pipes                          ( X
                                                , (>->)
                                                , await
                                                , yield
                                                )
import           Servant
import           Servant.Foreign
import           Servant.Foreign.Internal       ( _FunctionName )
import           Servant.JS.Internal
import           Servant.Pipes                  ( pipesToSourceIO )
import qualified Data.ByteString.Builder as BL
import qualified Data.ByteString.Lazy.Char8 as C8L
import qualified Data.Attoparsec.Combinator as A

newtype ServerSentEvents
  = ServerSentEvents (StreamGet NoFraming EventStream EventSourceHdr)
  deriving (Generic, HasLink)

instance HasServer ServerSentEvents context where
  type ServerT ServerSentEvents m = ServerT (StreamGet NoFraming EventStream EventSourceHdr) m
  route Proxy = route
    (Proxy :: Proxy (StreamGet NoFraming EventStream EventSourceHdr))
  hoistServerWithContext Proxy = hoistServerWithContext
    (Proxy :: Proxy (StreamGet NoFraming EventStream EventSourceHdr))

-- | A 'ServerEvent' parser that tries to follow the W3C spec available
-- [here](https://html.spec.whatwg.org/multipage/server-sent-events.html#server-sent-events).
serverEventParser :: A.Parser ServerEvent
serverEventParser = do

  -- Technically speaking, the server is not required to send clean \"packets\", but it
  -- might buffer and send us multiple parts of the \"data\" fragment. In other words, we
  -- might receive the initial \"data:\" field in one packet, and the rest in another, so
  -- we have to premptively try to handling this case.

  mb_field <- A.lookAhead (optional eventFieldParser)
  previousFrameLeftover <- case mb_field of
    Just "event" -> pure mempty
    Just "data"  -> pure mempty
    Just "id"    -> pure mempty
    _ -> C8.pack <$> (A.manyTill A8.anyChar (frameSeparator <|> A.endOfInput))

  eventName <- optional eventNameParser
  eventData <- addInitialFragment previousFrameLeftover
                 <$> A8.sepBy eventDataParser frameSeparator
  eventId   <- optional eventIdParser
  pure $ ServerEvent{..}

  where
    colon :: Word8
    colon = 58

    addInitialFragment :: C8.ByteString -> [BL.Builder] -> [BL.Builder]
    addInitialFragment d bs = if C8.null d then bs else B.fromByteString d : bs

    newline :: Word8
    newline = 10

    skipNewline :: A.Parser ()
    skipNewline = A.word8 newline *> pure ()

    frameSeparator :: A.Parser ()
    frameSeparator = skipNewline >> skipNewline

    eventFieldParser :: A.Parser C8.ByteString
    eventFieldParser = do
      field <- A.takeWhile ((/=) colon)
      _     <- A.word8 colon
      A8.skipSpace
      pure field

    eventNameParser :: A.Parser B.Builder
    eventNameParser = do
      field <- eventFieldParser
      -- Parse the event name
      case field of
        "event" -> B.fromByteString <$> (A.takeWhile ((/=) newline) <* skipNewline)
        _       -> fail "eventNameParser"

    eventIdParser :: A.Parser B.Builder
    eventIdParser = do
      field <- eventFieldParser
      -- Parse the event id
      case field of
        "id" -> B.fromByteString <$> (A.takeWhile ((/=) newline) <* skipNewline)
        _    -> fail "eventIdParser"

    eventDataParser :: A.Parser B.Builder
    eventDataParser = do
      field <- eventFieldParser
      case field of
        "data" -> B.fromByteString <$> A.takeWhile ((/=) newline)
        _      -> fail "eventDataParser"



instance MimeUnrender EventStream ServerEvent where
  mimeUnrender _ bs = do
    A.parseOnly serverEventParser (BSL8.toStrict bs)

-- | a helper instance for <https://hackage.haskell.org/package/servant-foreign-0.15.3/docs/Servant-Foreign.html servant-foreign>
instance  (HasForeignType lang ftype EventSourceHdr)
  => HasForeign lang ftype ServerSentEvents where
  type Foreign ftype ServerSentEvents = Req ftype

  foreignFor lang Proxy Proxy req =
    req
      &  reqFuncName .  _FunctionName %~ ("stream" :)
      &  reqMethod .~ method
      &  reqReturnType ?~ retType
   where
    retType = typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy EventSourceHdr)
    method  = reflectMethod (Proxy :: Proxy 'GET)

-- | A type representation of an event stream. It's responsible for setting proper content-type
--   and buffering headers, as well as for providing parser implementations for the streams.
--   Read more on <https://docs.servant.dev/en/stable/tutorial/Server.html#streaming-endpoints Servant Streaming Docs>
data EventStream

instance Accept EventStream where
  contentType _ = "text" // "event-stream" /: ("charset", "utf-8")

type EventSource = SourceIO ServerEvent

-- | This is mostly to guide reverse-proxies like 
--   <https://www.nginx.com/resources/wiki/start/topics/examples/x-accel/#x-accel-buffering nginx>
type EventSourceHdr = Headers '[Header "X-Accel-Buffering" Text] EventSource

-- | See details at
--   https://hackage.haskell.org/package/wai-extra-3.1.6/docs/Network-Wai-EventSource-EventStream.html#v:eventToBuilder
instance MimeRender EventStream ServerEvent where
  mimeRender _ = maybe "" toLazyByteString . eventToBuilder

eventSource :: Pipes.Proxy X () () ServerEvent IO () -> EventSourceHdr
eventSource prod = addHeader "no" $ pipesToSourceIO (prod >-> yieldUntilClose)
 where
  yieldUntilClose = do
    e <- await
    case e of
      CloseEvent -> return ()
      _          -> yield e >> yieldUntilClose

jsForAPI
  :: ( HasForeign NoTypes NoContent api
     , GenerateList NoContent (Foreign NoContent api)
     )
  => Proxy api
  -> Text
jsForAPI p = gen
  (listFromAPI (Proxy :: Proxy NoTypes) (Proxy :: Proxy NoContent) p)
 where
  gen :: [Req NoContent] -> Text
  gen = mconcat . map genEventSource

  genEventSource :: Req NoContent -> Text
  genEventSource req = T.unlines
    [ ""
    , fname <> " = function(" <> argsStr <> ")"
    , "{"
    , "  s = new EventSource(" <> url <> ", conf);"
    , "  Object.entries(eventListeners).forEach(([ev, cb]) => s.addEventListener(ev, cb));"
    , "  return s;"
    , "}"
    ]
   where
    argsStr = T.intercalate ", " args
    args = captures
        ++ map (view $ queryArgName . argPath) queryparams
        ++ ["eventListeners = {}", "conf"]

    captures = map (view argPath . captureArg)
              . filter isCapture
              $ req ^. reqUrl.path

    queryparams = req ^.. reqUrl.queryStr.traverse

    fname   = "var " <> toValidFunctionName (camelCase $ req ^. reqFuncName)
    url     = if url' == "'" then "'/'" else url'
    url'    = "'" <> urlArgs
    urlArgs = jsSegments $ req ^.. reqUrl . path . traverse

renderServerEvent :: ServerEvent -> T.Text
renderServerEvent = \case
  ServerEvent{..} ->
    let nm  = maybe "unnamed" builderToText eventName
        eid = maybe "no-id"   builderToText eventId
        bdy = T.intercalate "," (map builderToText eventData)
        in "ServerEvent<" <> nm <> "," <> eid <> ",[" <> bdy <> "]>"
  CommentEvent cmt -> "CommentEvent<" <> builderToText cmt <> ">"
  RetryEvent rtr   -> "RetryEvent<" <> T.pack (show rtr) <> ">"
  CloseEvent       -> "CloseEvent"

builderToText :: BL.Builder -> T.Text
builderToText = T.pack . C8L.unpack . BL.toLazyByteString
