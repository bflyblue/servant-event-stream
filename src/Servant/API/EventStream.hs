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
(SSE) support for Servant. Stream events from your server to connected clients,
or consume third-party SSE streams (such as
<https://platform.openai.com/docs/api-reference/streaming OpenAI>) from
Haskell.

== Sending events

Define a domain type for your events and a 'ToServerEvent' instance that maps
each constructor to an SSE event. Many streaming APIs encode the @data:@ field
as JSON — use 'jsonEvent' for this, or 'serverEvent' for raw bytestrings:

> data ChatEvent
>   = ContentDelta Text  -- ^ incremental text chunk
>   | ContentDone Text   -- ^ final assembled text
>   | ChatDone           -- ^ stream finished
>
> type MyApi = "chat" :> ServerSentEvents (SourceIO ChatEvent)
>
> instance ToServerEvent ChatEvent where
>   toServerEvent (ContentDelta d) = jsonEvent (Just "response.content.delta") Nothing d
>   toServerEvent (ContentDone t) = jsonEvent (Just "response.content.done") Nothing t
>   toServerEvent ChatDone = jsonEvent (Just "response.done") Nothing ()
>
> server :: Server MyApi
> server = pure $ source
>   [ContentDelta "Hel", ContentDelta "lo!", ContentDone "Hello!", ChatDone]

Some APIs accept a request body and respond with an event stream (e.g. OpenAI
chat completions). Use 'PostServerSentEvents' for these @POST@ endpoints:

> type MyApi = "chat" :> ReqBody '[JSON] ChatRequest :> PostServerSentEvents (SourceIO ChatEvent)

== Receiving events

To consume an SSE stream, provide a 'FromServerEvent' instance. Dispatch on
'eventType' to determine which constructor to use, and 'jsonData' to decode
JSON from the @data:@ field:

> instance FromServerEvent ChatEvent where
>   fromServerEvent ev = case eventType ev of
>     Just "response.content.delta" -> ContentDelta <$> jsonData ev
>     Just "response.content.done"  -> ContentDone <$> jsonData ev
>     Just "response.done"          -> Right ChatDone
>     _                             -> Left "unknown event"

== JSON via DerivingVia

When every event carries the same JSON structure (no @event:@ type dispatch),
t'JsonData' derives both 'ToServerEvent' and 'FromServerEvent' automatically:

> data Temperature = Temperature { celsius :: Double }
>   deriving (Generic, ToJSON, FromJSON)
>   deriving (ToServerEvent, FromServerEvent) via JsonData Temperature

== Reverse-proxy headers

Reverse proxies like nginx buffer responses by default, which prevents events
from reaching the client in real time. Wrap your stream in
'RecommendedEventSourceHeaders' to disable buffering:

> type MyApi = "chat" :> ServerSentEvents (RecommendedEventSourceHeaders (SourceIO ChatEvent))
-}
module Servant.API.EventStream (
  -- * API combinator
  ServerSentEvents,
  PostServerSentEvents,
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

  -- * JSON helpers
  JsonData (..),
  jsonEvent,
  jsonData,

  -- * Recommended headers
  RecommendedEventSourceHeaders,
  recommendedEventSourceHeaders,

  -- * Framing
  ServerEventFraming,
)
where

import Control.Lens ((%~), (&), (.~), (?~))
import Control.Monad ((<=<))
import qualified Data.Aeson as Aeson
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
import Network.HTTP.Media ((//))

import qualified Servant as S
import qualified Servant.Client.Core as SC
import qualified Servant.Foreign as SF
import qualified Servant.Foreign.Internal as SFI
import Servant.Types.SourceT (transformWithAtto)

-- ---------------------------------------------------------------------------
-- Content type

-- | The @text\/event-stream@ content type.
data EventStream

instance S.Accept EventStream where
  contentType _ = "text" // "event-stream"

-- ---------------------------------------------------------------------------
-- API combinator

{- | Use this in place of a @Verb@ in your API type to define an SSE endpoint.
Streams events to clients over a long-lived HTTP connection.
-}
data ServerSentEvents (a :: Type)
  deriving (Generic)

instance S.HasLink (ServerSentEvents a) where
  type MkLink (ServerSentEvents a) r = r
  toLink toA _ = toA

{- | Like 'ServerSentEvents' but for @POST@ endpoints. Use this when the
client sends a request body and the server responds with an event stream
(e.g. OpenAI's streaming API).
-}
data PostServerSentEvents (a :: Type)
  deriving (Generic)

instance S.HasLink (PostServerSentEvents a) where
  type MkLink (PostServerSentEvents a) r = r
  toLink toA _ = toA

-- ---------------------------------------------------------------------------
-- Events

-- | A single SSE event — the intermediate representation between your domain types and the wire format.
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

{- | Construct an event with a type, id, and data payload.
Mirrors the pre-0.4 @ServerEvent@ constructor for easy migration.
-}
serverEvent :: Maybe LBS.ByteString -> Maybe LBS.ByteString -> LBS.ByteString -> ServerEvent
serverEvent typ eid dat = ServerEvent typ eid dat Nothing Nothing

-- | An event carrying only a data payload, with no event type or id.
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

-- | Convert a domain type to a t'ServerEvent' for sending over the wire.
class ToServerEvent a where
  toServerEvent :: a -> ServerEvent

instance ToServerEvent ServerEvent where
  toServerEvent = id

instance (ToServerEvent a) => S.MimeRender EventStream a where
  mimeRender _ = encodeServerEvent . toServerEvent

-- | Encode a t'ServerEvent' to its wire format. Called automatically by 'S.MimeRender'; use directly only for custom encoding needs.
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

-- | Parse a received t'ServerEvent' back into a domain type.
class FromServerEvent a where
  fromServerEvent :: ServerEvent -> Either String a

instance FromServerEvent ServerEvent where
  fromServerEvent = Right

instance (FromServerEvent a) => S.MimeUnrender EventStream a where
  mimeUnrender _ = fromServerEvent <=< decodeServerEvent

{- | Decode a single SSE event block from its wire format. Called automatically
by 'S.MimeUnrender'; use directly only for custom decoding needs.

Parsing follows the
<https://html.spec.whatwg.org/multipage/server-sent-events.html#event-stream-interpretation WHATWG SSE spec>.
-}
decodeServerEvent :: LBS.ByteString -> Either String ServerEvent
decodeServerEvent input =
  let ls = splitLines (stripBOM (LBS.toStrict input))
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
-- JSON helpers

{- | Derive 'ToServerEvent' and 'FromServerEvent' for types that serialise
entirely as JSON in the @data:@ field, with no @event:@ type or @id:@.

For sum types that dispatch on 'eventType', write manual instances using
'jsonEvent' and 'jsonData' instead.

@
data Temperature = Temperature { celsius :: Double }
  deriving (ToJSON, FromJSON)
  deriving (ToServerEvent, FromServerEvent) via JsonData Temperature
@
-}
newtype JsonData a = JsonData {unJsonData :: a}

instance (Aeson.ToJSON a) => ToServerEvent (JsonData a) where
  toServerEvent (JsonData a) = jsonEvent Nothing Nothing a

instance (Aeson.FromJSON a) => FromServerEvent (JsonData a) where
  fromServerEvent = fmap JsonData . jsonData

{- | Construct a t'ServerEvent' with a JSON-encoded @data:@ payload.
The encoding counterpart to 'jsonData'.

@
instance ToServerEvent ChatEvent where
    toServerEvent (ContentDelta d) = jsonEvent (Just \"response.content.delta\") Nothing d
    toServerEvent ChatDone         = jsonEvent (Just \"response.done\") Nothing ()
@
-}
jsonEvent :: (Aeson.ToJSON a) => Maybe LBS.ByteString -> Maybe LBS.ByteString -> a -> ServerEvent
jsonEvent typ eid a = serverEvent typ eid (Aeson.encode a)

{- | Decode the 'eventData' field as JSON.
The decoding counterpart to 'jsonEvent'.

@
instance FromServerEvent ChatEvent where
    fromServerEvent ev = case eventType ev of
        Just \"response.content.delta\" -> ContentDelta \<$\> jsonData ev
        Just \"response.done\"          -> Right ChatDone
        _                             -> Left \"unknown event\"
@
-}
jsonData :: (Aeson.FromJSON a) => ServerEvent -> Either String a
jsonData = Aeson.eitherDecode . eventData

-- | Strip a leading UTF-8 BOM (@\\xEF\\xBB\\xBF@) if present, per the WHATWG SSE spec.
stripBOM :: BS.ByteString -> BS.ByteString
stripBOM bs
  | "\xEF\xBB\xBF" `BS.isPrefixOf` bs = BS.drop 3 bs
  | otherwise = bs

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

-- | Wrap a streaming response with the recommended headers. See 'RecommendedEventSourceHeaders'.
recommendedEventSourceHeaders :: a -> RecommendedEventSourceHeaders a
recommendedEventSourceHeaders = S.addHeader @"X-Accel-Buffering" "no" . S.addHeader @"Cache-Control" "no-store"

-- ---------------------------------------------------------------------------
-- Framing

-- | SSE framing strategy. Applied automatically by 'ServerSentEvents' and 'PostServerSentEvents'.
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
      let block = stripBOM (BS.intercalate "\n" ls <> "\n")
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

instance {-# OVERLAPPABLE #-} (ToServerEvent chunk, S.ToSourceIO chunk a) => S.HasServer (PostServerSentEvents a) context where
  type ServerT (PostServerSentEvents a) m = S.ServerT (S.StreamPost ServerEventFraming EventStream a) m
  route S.Proxy =
    S.route
      (S.Proxy :: S.Proxy (S.StreamPost ServerEventFraming EventStream a))
  hoistServerWithContext S.Proxy =
    S.hoistServerWithContext
      (S.Proxy :: S.Proxy (S.StreamPost ServerEventFraming EventStream a))

instance {-# OVERLAPPING #-} (ToServerEvent chunk, S.ToSourceIO chunk a, S.GetHeaders (S.Headers h a)) => S.HasServer (PostServerSentEvents (S.Headers h a)) context where
  type ServerT (PostServerSentEvents (S.Headers h a)) m = S.ServerT (S.StreamPost ServerEventFraming EventStream (S.Headers h a)) m
  route S.Proxy =
    S.route
      (S.Proxy :: S.Proxy (S.StreamPost ServerEventFraming EventStream (S.Headers h a)))
  hoistServerWithContext S.Proxy =
    S.hoistServerWithContext
      (S.Proxy :: S.Proxy (S.StreamPost ServerEventFraming EventStream (S.Headers h a)))

instance {-# OVERLAPPABLE #-} (FromServerEvent chunk, SC.RunStreamingClient m, S.FromSourceIO chunk a) => SC.HasClient m (ServerSentEvents a) where
  type Client m (ServerSentEvents a) = SC.Client m (S.StreamGet ServerEventFraming EventStream a)
  clientWithRoute pm S.Proxy =
    SC.clientWithRoute
      pm
      (S.Proxy :: S.Proxy (S.StreamGet ServerEventFraming EventStream a))
  hoistClientMonad pm S.Proxy =
    SC.hoistClientMonad
      pm
      (S.Proxy :: S.Proxy (S.StreamGet ServerEventFraming EventStream a))

instance {-# OVERLAPPING #-} (FromServerEvent chunk, SC.RunStreamingClient m, S.FromSourceIO chunk a, S.BuildHeadersTo h) => SC.HasClient m (ServerSentEvents (S.Headers h a)) where
  type Client m (ServerSentEvents (S.Headers h a)) = SC.Client m (S.StreamGet ServerEventFraming EventStream (S.Headers h a))
  clientWithRoute pm S.Proxy =
    SC.clientWithRoute
      pm
      (S.Proxy :: S.Proxy (S.StreamGet ServerEventFraming EventStream (S.Headers h a)))
  hoistClientMonad pm S.Proxy =
    SC.hoistClientMonad
      pm
      (S.Proxy :: S.Proxy (S.StreamGet ServerEventFraming EventStream (S.Headers h a)))

instance {-# OVERLAPPABLE #-} (FromServerEvent chunk, SC.RunStreamingClient m, S.FromSourceIO chunk a) => SC.HasClient m (PostServerSentEvents a) where
  type Client m (PostServerSentEvents a) = SC.Client m (S.StreamPost ServerEventFraming EventStream a)
  clientWithRoute pm S.Proxy =
    SC.clientWithRoute
      pm
      (S.Proxy :: S.Proxy (S.StreamPost ServerEventFraming EventStream a))
  hoistClientMonad pm S.Proxy =
    SC.hoistClientMonad
      pm
      (S.Proxy :: S.Proxy (S.StreamPost ServerEventFraming EventStream a))

instance {-# OVERLAPPING #-} (FromServerEvent chunk, SC.RunStreamingClient m, S.FromSourceIO chunk a, S.BuildHeadersTo h) => SC.HasClient m (PostServerSentEvents (S.Headers h a)) where
  type Client m (PostServerSentEvents (S.Headers h a)) = SC.Client m (S.StreamPost ServerEventFraming EventStream (S.Headers h a))
  clientWithRoute pm S.Proxy =
    SC.clientWithRoute
      pm
      (S.Proxy :: S.Proxy (S.StreamPost ServerEventFraming EventStream (S.Headers h a)))
  hoistClientMonad pm S.Proxy =
    SC.hoistClientMonad
      pm
      (S.Proxy :: S.Proxy (S.StreamPost ServerEventFraming EventStream (S.Headers h a)))

-- | Enables <https://hackage.haskell.org/package/servant-foreign servant-foreign> code generation, prefixing function names with \"stream\".
instance
  (SF.HasForeignType lang ftype a) =>
  SF.HasForeign lang ftype (ServerSentEvents a)
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

-- | Enables <https://hackage.haskell.org/package/servant-foreign servant-foreign> code generation for 'PostServerSentEvents'.
instance
  (SF.HasForeignType lang ftype a) =>
  SF.HasForeign lang ftype (PostServerSentEvents a)
  where
  type Foreign ftype (PostServerSentEvents a) = SFI.Req ftype

  foreignFor lang S.Proxy S.Proxy req =
    req
      & SFI.reqFuncName . SFI._FunctionName %~ ("stream" :)
      & SFI.reqMethod .~ method
      & SFI.reqReturnType ?~ retType
   where
    retType = SFI.typeFor lang (S.Proxy :: S.Proxy ftype) (S.Proxy :: S.Proxy a)
    method = S.reflectMethod (S.Proxy :: S.Proxy S.POST)
