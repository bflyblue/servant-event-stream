{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module: Servant.API.EventStream
Description: Server-Sent Events for Servant
Copyright: (c) 2026 Shaun Sharples
License: BSD3
Stability: alpha

<https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events Server-Sent Events>
(SSE) support for Servant. Provides a 'ServerSentEvents' API combinator and
a 'ToServerEvent' typeclass so you can stream custom event types from Servant
endpoints.

== Sending events

Use 'ServerSentEvents' in your API type and provide a 'ToServerEvent' instance
for your domain type. Each constructor maps to an SSE @event:@ type, and the
payload is carried in the @data:@ field:

> data ChatEvent
>   = ContentDelta ByteString  -- ^ incremental text chunk
>   | ContentDone ByteString   -- ^ final assembled text
>   | ChatDone                 -- ^ stream finished
>
> type MyApi = "chat" :> ServerSentEvents (SourceIO ChatEvent)
>
> instance ToServerEvent ChatEvent where
>   toServerEvent (ContentDelta d) =
>     serverEvent (Just "response.content.delta") Nothing d
>   toServerEvent (ContentDone t) =
>     serverEvent (Just "response.content.done") Nothing t
>   toServerEvent ChatDone =
>     serverEvent (Just "response.done") Nothing ""
>
> server :: Server MyApi
> server = pure $ source
>   [ContentDelta "Hel", ContentDelta "lo!", ContentDone "Hello!", ChatDone]

== Receiving events

On the client side, provide a 'FromServerEvent' instance to parse incoming
events back into your domain type. Dispatch on 'eventType' to determine which
constructor to use. This is particularly useful for consuming third-party SSE
APIs such as OpenAI:

> instance FromServerEvent ChatEvent where
>   fromServerEvent ev = case eventType ev of
>     Just "response.content.delta" -> Right (ContentDelta (eventData ev))
>     Just "response.content.done"  -> Right (ContentDone (eventData ev))
>     Just "response.done"          -> Right ChatDone
>     _                             -> Left "unknown event"

== Reverse-proxy headers

Wrap your stream in 'RecommendedEventSourceHeaders' to add @X-Accel-Buffering@
and @Cache-Control@ headers that prevent reverse proxies from buffering the
event stream:

> type MyApi = "chat" :> ServerSentEvents (RecommendedEventSourceHeaders (SourceIO ChatEvent))
-}
module Servant.API.EventStream (
    -- * API combinator
    ServerSentEvents,
    EventStream,

    -- * Events
    ServerEvent (..),
    serverEvent,
    dataEvent,
    commentEvent,
    retryEvent,

    -- * Sending events
    ToServerEvent (..),
    encodeServerEvent,

    -- * Receiving events
    FromServerEvent (..),
    decodeServerEvent,

    -- * Recommended headers
    RecommendedEventSourceHeaders,
    recommendedEventSourceHeaders,

    -- * Framing
    ServerEventFraming,
)
where

import Control.Lens
import Control.Monad ((<=<))
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8S
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Char (digitToInt, isDigit)
import Data.Kind (Type)
#if !MIN_VERSION_base(4,20,0)
import Data.List (foldl')
#endif
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Media ((//), (/:))
import qualified Servant as S
import qualified Servant.Foreign as S
import qualified Servant.Foreign.Internal as SFI
import Servant.Types.SourceT (transformWithAtto)

-- ---------------------------------------------------------------------------
-- Content type

-- | The @text\/event-stream@ content type.
data EventStream

instance S.Accept EventStream where
    contentType _ = "text" // "event-stream" /: ("charset", "utf-8")

-- ---------------------------------------------------------------------------
-- API combinator

{- | A Servant API combinator for
<https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events Server-Sent Event>
endpoints. Use this in place of a @Verb@ to stream events to clients.
-}
data ServerSentEvents (a :: Type)
    deriving (Generic)

instance S.HasLink (ServerSentEvents a) where
    type MkLink (ServerSentEvents a) r = r
    toLink toA _ = toA

-- ---------------------------------------------------------------------------
-- Events

-- | An SSE event. Corresponds to a single event block in the wire format.
data ServerEvent = ServerEvent
    { eventType :: !(Maybe LBS.ByteString)
    -- ^ The @event:@ field, used by clients to dispatch via @addEventListener@.
    , eventId :: !(Maybe LBS.ByteString)
    -- ^ The @id:@ field. Sent as @Last-Event-ID@ on reconnection.
    , eventData :: !LBS.ByteString
    -- ^ The @data:@ payload. Multi-line values are split across multiple @data:@ fields on the wire.
    , eventComment :: !(Maybe LBS.ByteString)
    -- ^ A @:@ comment line. Commonly used as a keepalive heartbeat.
    , eventRetry :: !(Maybe Word)
    -- ^ The @retry:@ field — reconnection delay in milliseconds.
    }
    deriving (Show, Eq, Generic)

{- | Construct an event with an optional type, optional id, and data payload.
This mirrors the pre-0.4 @ServerEvent@ constructor for easy migration.
-}
serverEvent :: Maybe LBS.ByteString -> Maybe LBS.ByteString -> LBS.ByteString -> ServerEvent
serverEvent typ eid dat = ServerEvent typ eid dat Nothing Nothing

-- | Construct a simple event carrying only a data payload.
dataEvent :: LBS.ByteString -> ServerEvent
dataEvent dat = ServerEvent Nothing Nothing dat Nothing Nothing

-- | Construct a comment-only event, useful as a keepalive heartbeat.
commentEvent :: LBS.ByteString -> ServerEvent
commentEvent c = ServerEvent Nothing Nothing "" (Just c) Nothing

-- | Construct a retry event that sets the client's reconnection delay in milliseconds.
retryEvent :: Word -> ServerEvent
retryEvent ms = ServerEvent Nothing Nothing "" Nothing (Just ms)

-- ---------------------------------------------------------------------------
-- Sending events

-- | Convert a custom type to a t'ServerEvent' for sending.
class ToServerEvent a where
    toServerEvent :: a -> ServerEvent

instance ToServerEvent ServerEvent where
    toServerEvent = id

instance (ToServerEvent a) => S.MimeRender EventStream a where
    mimeRender _ = encodeServerEvent . toServerEvent

-- | Encode a t'ServerEvent' to its wire format.
encodeServerEvent :: ServerEvent -> LBS.ByteString
encodeServerEvent e =
    optional ":" (sanitize <$> eventComment e)
        <> maybe mempty (\ms -> "retry: " <> C8.pack (show ms) <> "\n") (eventRetry e)
        <> optional "event:" (sanitize <$> eventType e)
        <> optional "id:" (sanitizeId <$> eventId e)
        <> mconcat (map (field "data:") (safedata (eventData e)))
  where
    optional name = maybe mempty (field name)
    field name val = name <> " " <> val <> "\n"

    sanitize = C8.filter (\c -> c /= '\r' && c /= '\n')
    sanitizeId = C8.filter (\c -> c /= '\r' && c /= '\n' && c /= '\0')

    safedata bs = case safelines bs of
        [] -> [""]
        xs -> xs
    safelines = C8.lines . C8.filter (/= '\r')

-- ---------------------------------------------------------------------------
-- Receiving events

-- | Parse a t'ServerEvent' into a custom type after receiving.
class FromServerEvent a where
    fromServerEvent :: ServerEvent -> Either String a

instance FromServerEvent ServerEvent where
    fromServerEvent = Right

instance (FromServerEvent a) => S.MimeUnrender EventStream a where
    mimeUnrender _ = fromServerEvent <=< decodeServerEvent

{- | Decode a single SSE event block from its wire format.

Parsing follows the
<https://html.spec.whatwg.org/multipage/server-sent-events.html#event-stream-interpretation WHATWG SSE spec>.
-}
decodeServerEvent :: LBS.ByteString -> Either String ServerEvent
decodeServerEvent input =
    let ls = splitLines (LBS.toStrict input)
        (typ, eid, dataParts, comment, retry) = foldl' processLine (Nothing, Nothing, [], Nothing, Nothing) ls
        dat = case dataParts of
            [] -> ""
            _ -> LBS.fromStrict (BS.intercalate "\n" (reverse dataParts))
     in Right
            ServerEvent
                { eventType = LBS.fromStrict <$> typ
                , eventId = LBS.fromStrict <$> eid
                , eventData = dat
                , eventComment = LBS.fromStrict <$> comment
                , eventRetry = retry
                }
  where
    processLine acc@(typ, eid, dataParts, comment, retry) line
        | BS.null line = acc -- skip empty lines
        | C8S.head line == ':' =
            let val = stripOneSpace (BS.drop 1 line)
             in (typ, eid, dataParts, Just val, retry)
        | otherwise =
            let (name, val) = case C8S.elemIndex ':' line of
                    Just i -> (BS.take i line, stripOneSpace (BS.drop (i + 1) line))
                    Nothing -> (line, "")
             in if
                    | name == "event" -> (Just val, eid, dataParts, comment, retry)
                    | name == "data" -> (typ, eid, val : dataParts, comment, retry)
                    | name == "id" ->
                        if C8S.elem '\0' val
                            then acc
                            else (typ, Just val, dataParts, comment, retry)
                    | name == "retry" ->
                        if BS.null val || not (C8S.all isDigit val)
                            then acc
                            else (typ, eid, dataParts, comment, Just (parseWord val))
                    | otherwise -> acc

    stripOneSpace bs
        | not (BS.null bs) && C8S.head bs == ' ' = BS.drop 1 bs
        | otherwise = bs

    parseWord = C8S.foldl' (\n c -> n * 10 + fromIntegral (digitToInt c)) 0

    splitLines bs = map stripCR (C8S.split '\n' bs)
      where
        stripCR s
            | BS.null s = s
            | C8S.last s == '\r' = BS.init s
            | otherwise = s

-- ---------------------------------------------------------------------------
-- Recommended headers

{- | Adds @X-Accel-Buffering: no@ and @Cache-Control: no-store@ headers to
prevent reverse proxies (e.g. nginx) from buffering the event stream.

> type MyApi = "events" :> ServerSentEvents (RecommendedEventSourceHeaders (SourceIO Event))
>
> server :: Server MyApi
> server = pure $ recommendedEventSourceHeaders $ source [event1, event2]
-}
type RecommendedEventSourceHeaders (a :: Type) = S.Headers '[S.Header "X-Accel-Buffering" Text, S.Header "Cache-Control" Text] a

-- | Add the recommended headers to a response.
recommendedEventSourceHeaders :: a -> RecommendedEventSourceHeaders a
recommendedEventSourceHeaders = S.addHeader @"X-Accel-Buffering" "no" . S.addHeader @"Cache-Control" "no-store"

-- ---------------------------------------------------------------------------
-- Framing

-- | Separates events with blank lines per the SSE wire format.
data ServerEventFraming

instance S.FramingRender ServerEventFraming where
    framingRender _ f = fmap (\x -> f x <> "\n")

instance S.FramingUnrender ServerEventFraming where
    framingUnrender _ f = transformWithAtto (eventParser f)

eventParser :: (LBS.ByteString -> Either String a) -> A.Parser a
eventParser f = do
    ls <- collectLines
    case ls of
        [] -> fail "empty event"
        _ -> do
            let block = BS.intercalate "\n" ls <> "\n"
            case f (LBS.fromStrict block) of
                Left err -> fail err
                Right a -> pure a
  where
    collectLines = do
        atEnd <- A.atEnd
        if atEnd
            then pure []
            else do
                line <- A.takeWhile (\c -> c /= '\n' && c /= '\r')
                -- consume line ending: CRLF, CR, or LF
                _ <- A.option () (A.char '\r' *> pure ())
                _ <- A.option () (A.char '\n' *> pure ())
                if BS.null line
                    then pure []
                    else do
                        rest <- collectLines
                        pure (line : rest)

-- ---------------------------------------------------------------------------
-- Servant integration

instance {-# OVERLAPPABLE #-} (ToServerEvent chunk, S.ToSourceIO chunk a) => S.HasServer (ServerSentEvents a) context where
    type ServerT (ServerSentEvents a) m = S.ServerT (S.StreamGet ServerEventFraming EventStream a) m
    route S.Proxy =
        S.route
            (S.Proxy :: S.Proxy (S.StreamGet ServerEventFraming EventStream a))
    hoistServerWithContext S.Proxy =
        S.hoistServerWithContext
            (S.Proxy :: S.Proxy (S.StreamGet ServerEventFraming EventStream a))

instance {-# OVERLAPPING #-} (ToServerEvent chunk, S.ToSourceIO chunk a, S.GetHeaders (S.Headers h a)) => S.HasServer (ServerSentEvents (S.Headers h a)) context where
    type ServerT (ServerSentEvents (S.Headers h a)) m = S.ServerT (S.StreamGet ServerEventFraming EventStream (S.Headers h a)) m
    route S.Proxy =
        S.route
            (S.Proxy :: S.Proxy (S.StreamGet ServerEventFraming EventStream (S.Headers h a)))
    hoistServerWithContext S.Proxy =
        S.hoistServerWithContext
            (S.Proxy :: S.Proxy (S.StreamGet ServerEventFraming EventStream (S.Headers h a)))

-- | Enables <https://hackage.haskell.org/package/servant-foreign servant-foreign> code generation, prefixing function names with \"stream\".
instance
    (S.HasForeignType lang ftype a) =>
    S.HasForeign lang ftype (ServerSentEvents a)
    where
    type Foreign ftype (ServerSentEvents a) = SFI.Req ftype

    foreignFor lang S.Proxy S.Proxy req =
        req
            & SFI.reqFuncName . SFI._FunctionName %~ ("stream" :)
            & SFI.reqMethod .~ method
            & SFI.reqReturnType ?~ retType
      where
        retType = SFI.typeFor lang (S.Proxy :: S.Proxy ftype) (S.Proxy :: S.Proxy a)
        method = S.reflectMethod (S.Proxy :: S.Proxy S.GET)
