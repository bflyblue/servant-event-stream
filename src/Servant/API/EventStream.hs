{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module: Servant.API.EventStream
Description: Server Sent Events for Servant Streams
Copyright: (c) 2024 Shaun Sharples
License: BSD3
Stability: alpha
-}
module Servant.API.EventStream (
  -- * Server-Sent Events

  -- | Event streams are implemented using servant's 'Stream' endpoint.
  -- You should provide a handler that returns a stream of events that implements
  -- 'ToSourceIO' where events have a 'ToServerEvent' instance.
  --
  -- Example:
  --
  -- > type MyApi = "books" :> ServerSentEvents (SourceIO Book)
  -- >
  -- > instance ToServerEvent Book where
  -- >   toServerEvent book = ...
  -- >
  -- > server :: Server MyApi
  -- > server = streamBooks
  -- >   where streamBooks :: Handler (SourceIO Book)
  -- >         streamBooks = pure $ source [book1, ...]
  ServerEvent (..),
  ToServerEvent (..),
  FromServerEvent (..),
  ServerSentEvents,
  EventStream,
  ServerEventFraming,

  -- * Recommended headers for Server-Sent Events

  -- | This is mostly to guide reverse-proxies like
  --   <https://www.nginx.com/resources/wiki/start/topics/examples/x-accel/#x-accel-buffering nginx>.
  --
  --   Example:
  --
  --   > type MyApi = "books" :> ServerSentEvents (RecommendedEventSourceHeaders (SourceIO Book))
  --   >
  --   > server :: Server MyApi
  --   > server = streamBooks
  --   >   where streamBooks :: Handler (RecommendedEventSourceHeaders (SourceIO Book))
  --   >         streamBooks = pure $ recommendedEventSourceHeaders $ source [book1, ...]
  RecommendedEventSourceHeaders,
  recommendedEventSourceHeaders,
)
where

import Control.Applicative (optional, (<|>))
import Control.Lens ((%~), (&), (.~), (?~))
import Control.Monad (void, (<=<))
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.Text as AT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Kind (Type)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Network.HTTP.Media ((//), (/:))
import Servant (
  Accept (contentType),
  FramingRender (..),
  FramingUnrender (..),
  GetHeaders,
  HasLink (..),
  HasServer (..),
  Header,
  Headers,
  MimeRender (mimeRender),
  MimeUnrender (mimeUnrender),
  Proxy (..),
  StdMethod (GET),
  StreamGet,
  ToSourceIO,
  addHeader,
  reflectMethod,
 )
import Servant.Foreign (
  HasForeign (..),
  HasForeignType (..),
  Req,
  reqFuncName,
  reqMethod,
  reqReturnType,
 )
import Servant.Foreign.Internal (_FunctionName)
import Servant.Types.SourceT (transformWithAtto)
import Prelude

{- | A ServerSentEvents endpoint emits an event stream using the format described at
  <https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#event_stream_format>
-}
data ServerSentEvents (a :: Type)
  deriving (Typeable, Generic)

instance HasLink (ServerSentEvents a) where
  type MkLink (ServerSentEvents a) r = r
  toLink toA _ = toA

-- | Represents an event sent from the server to the client in Server-Sent Events (SSE).
data ServerEvent = ServerEvent
  { eventType :: !(Maybe Text)
  -- ^ Optional field specifying the type of event. Can be used to distinguish between different kinds of events.
  , eventId :: !(Maybe Text)
  -- ^ Optional field providing an identifier for the event. Useful for clients to keep track of the last received event.
  , eventData :: !Text
  -- ^ The payload or content of the event. This is the main data sent to the client.
  }
  deriving (Show, Eq, Generic)

{- | This typeclass allows you to define custom event types that can be
  transformed into the t'ServerEvent' type, which is used to represent events in
  the Server-Sent Events (SSE) protocol.
-}
class ToServerEvent a where
  toServerEvent :: a -> ServerEvent

instance (ToServerEvent a) => MimeRender EventStream a where
  mimeRender _ = encodeServerEvent . toServerEvent

{- | This typeclass allow you to define custom event types that can be interpreted
from a t'ServerEvent' type.
-}
class FromServerEvent a where
  fromServerEvent :: ServerEvent -> Either String a

instance (FromServerEvent a) => MimeUnrender EventStream a where
  mimeUnrender _ = fromServerEvent <=< decodeServerEvent

{- 1. Field names must not contain LF, CR or COLON characters.
   2. Values must not contain LF or CR characters.
      Multple consecutive `data:` fields will be joined with LFs on the client.
-}

-- | Encodes a t'ServerEvent' into a 'LBS.ByteString' that can be sent to the client.
encodeServerEvent :: ServerEvent -> LBS.ByteString
encodeServerEvent e =
  optionalField "event:" (eventType e)
    <> optionalField "id:" (eventId e)
    <> mconcat (map (field "data:") (safelines (eventData e)))
 where
  optionalField name = maybe mempty (field name)
  field name val = name <> LBS.fromStrict (encodeUtf8 val) <> "\n"

  -- discard CR and split LFs into multiple data values
  safelines = Text.lines . Text.filter (/= '\r')

decodeServerEvent :: LBS.ByteString -> Either String ServerEvent
decodeServerEvent bs = do
  decodedText <- first show $ decodeUtf8' (LBS.toStrict bs)
  f <- AT.parseOnly linesParser decodedText
  pure $ f emptyEvent
 where
  emptyEvent = ServerEvent{eventType = Nothing, eventId = Nothing, eventData = ""}

  linesParser = foldr (.) id <$> AT.many' lineParser

  lineParser = do
    line <- parseLine
    case line of
      BlankLine -> pure id
      CommentLine -> pure id
      FieldLine field value -> pure $ processField field value

data Line = BlankLine | CommentLine | FieldLine !Text !Text
  deriving (Show, Eq)

endOfLine :: AT.Parser ()
endOfLine = void $ AT.choice [AT.string "\r\n", AT.string "\n", AT.string "\r"]

isEndOfLine :: Char -> Bool
isEndOfLine '\n' = True
isEndOfLine '\r' = True
isEndOfLine _ = False

parseLine :: AT.Parser Line
parseLine =
  AT.choice
    [ BlankLine <$ endOfLine
    , CommentLine <$ (AT.char ':' >> AT.skipWhile (not . isEndOfLine) >> endOfLine)
    , FieldLine <$> AT.takeWhile1 (/= ':') <* AT.char ':' <* optional AT.space <*> AT.takeWhile (not . isEndOfLine) <* AT.endOfLine
    ]

processField :: Text -> Text -> ServerEvent -> ServerEvent
processField "event" value event = event{eventType = Just value}
processField "data" value event = event{eventData = eventData event <> value <> "\n"}
processField "id" value event
  | Text.any (== '\0') value = event
  | otherwise = event{eventId = Just value}
processField _ _ event = event

{- TODO: retry
  If the field value consists of only ASCII digits, then interpret the field value as an integer in base ten, and set the event stream's reconnection time to that integer. Otherwise, ignore the field.
  processField "retry" value event = ?
-}

instance ToServerEvent ServerEvent where
  toServerEvent = id

instance {-# OVERLAPPABLE #-} (ToServerEvent chunk, ToSourceIO chunk a) => HasServer (ServerSentEvents a) context where
  type ServerT (ServerSentEvents a) m = ServerT (StreamGet ServerEventFraming EventStream a) m
  route Proxy =
    route
      (Proxy :: Proxy (StreamGet ServerEventFraming EventStream a))
  hoistServerWithContext Proxy =
    hoistServerWithContext
      (Proxy :: Proxy (StreamGet ServerEventFraming EventStream a))

instance {-# OVERLAPPING #-} (ToServerEvent chunk, ToSourceIO chunk a, GetHeaders (Headers h a)) => HasServer (ServerSentEvents (Headers h a)) context where
  type ServerT (ServerSentEvents (Headers h a)) m = ServerT (StreamGet ServerEventFraming EventStream (Headers h a)) m
  route Proxy =
    route
      (Proxy :: Proxy (StreamGet ServerEventFraming EventStream (Headers h a)))
  hoistServerWithContext Proxy =
    hoistServerWithContext
      (Proxy :: Proxy (StreamGet ServerEventFraming EventStream (Headers h a)))

-- | a helper instance for <https://hackage.haskell.org/package/servant-foreign-0.15.3/docs/Servant-Foreign.html servant-foreign>
instance
  (HasForeignType lang ftype a) =>
  HasForeign lang ftype (ServerSentEvents a)
  where
  type Foreign ftype (ServerSentEvents a) = Req ftype

  foreignFor lang Proxy Proxy req =
    req
      & reqFuncName . _FunctionName %~ ("stream" :)
      & reqMethod .~ method
      & reqReturnType ?~ retType
   where
    retType = typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy a)
    method = reflectMethod (Proxy :: Proxy 'GET)

{- | A type representation of an event stream. It's responsible for setting proper content-type
  and buffering headers, as well as for providing parser implementations for the streams.
  Read more on <https://docs.servant.dev/en/stable/tutorial/Server.html#streaming-endpoints Servant Streaming Docs>
-}
data EventStream

instance Accept EventStream where
  contentType _ = "text" // "event-stream" /: ("charset", "utf-8")

-- | Recommended headers for Server-Sent Events.
type RecommendedEventSourceHeaders (a :: Type) = Headers '[Header "X-Accel-Buffering" Text, Header "Cache-Control" Text] a

-- | Add the recommended headers for Server-Sent Events to the response.
recommendedEventSourceHeaders :: a -> RecommendedEventSourceHeaders a
recommendedEventSourceHeaders = addHeader @"X-Accel-Buffering" "no" . addHeader @"Cache-Control" "no-store"

-- | A framing strategy for Server-Sent Events.
data ServerEventFraming

-- | Frames the server events by joining chunks with a newline.
instance FramingRender ServerEventFraming where
  framingRender _ f = fmap (\x -> f x <> "\n")

newlineBS :: AB.Parser BS.ByteString
newlineBS = AB.choice [AB.string "\r\n", AB.string "\n", AB.string "\r"]

instance FramingUnrender ServerEventFraming where
  framingUnrender _ f = transformWithAtto $ do
    ws <- AB.manyTill AB.anyWord8 (AB.endOfInput <|> void (newlineBS >> newlineBS))
    case ws of
      [] -> fail "Unexpected empty frame"
      _ -> either fail pure (f (LBS.pack (ws <> [10])))
