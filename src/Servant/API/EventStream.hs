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
Copyright: (c) 2026 Shaun Sharples
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
  ServerSentEvents,
  EventStream,

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

import Control.Lens
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Kind (Type)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Media ((//), (/:))
import qualified Servant as S
import qualified Servant.Foreign as S
import qualified Servant.Foreign.Internal as SFI

{- | A ServerSentEvents endpoint emits an event stream using the format described at
  <https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#event_stream_format>
-}
data ServerSentEvents (a :: Type)
  deriving (Generic)

instance S.HasLink (ServerSentEvents a) where
  type MkLink (ServerSentEvents a) r = r
  toLink toA _ = toA

-- | Represents an event sent from the server to the client in Server-Sent Events (SSE).
data ServerEvent = ServerEvent
  { eventType :: !(Maybe LBS.ByteString)
  -- ^ Optional field specifying the type of event. Can be used to distinguish between different kinds of events.
  , eventId :: !(Maybe LBS.ByteString)
  -- ^ Optional field providing an identifier for the event. Useful for clients to keep track of the last received event.
  , eventData :: !LBS.ByteString
  -- ^ The payload or content of the event. This is the main data sent to the client.
  }
  deriving (Show, Eq, Generic)

{- | This typeclass allows you to define custom event types that can be
  transformed into the t'ServerEvent' type, which is used to represent events in
  the Server-Sent Events (SSE) protocol.
-}
class ToServerEvent a where
  toServerEvent :: a -> ServerEvent

instance (ToServerEvent a) => S.MimeRender EventStream a where
  mimeRender _ = encodeServerEvent . toServerEvent

{- 1. Field names must not contain LF, CR or COLON characters.
   2. Values must not contain LF or CR characters.
      Multple consecutive `data:` fields will be joined with LFs on the client.
-}

-- | Encodes a t'ServerEvent' into a 'LBS.ByteString' that can be sent to the client.
encodeServerEvent :: ServerEvent -> LBS.ByteString
encodeServerEvent e =
  optional "event:" (eventType e)
    <> optional "id:" (eventId e)
    <> mconcat (map (field "data:") (safelines (eventData e)))
 where
  optional name = maybe mempty (field name)
  field name val = name <> val <> "\n"

  -- discard CR and split LFs into multiple data values
  safelines = C8.lines . C8.filter (/= '\r')

instance ToServerEvent ServerEvent where
  toServerEvent = id

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

-- | a helper instance for <https://hackage.haskell.org/package/servant-foreign-0.15.3/docs/Servant-Foreign.html servant-foreign>
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

{- | A type representation of an event stream. It's responsible for setting proper content-type
  and buffering headers, as well as for providing parser implementations for the streams.
  Read more on <https://docs.servant.dev/en/stable/tutorial/Server.html#streaming-endpoints Servant Streaming Docs>
-}
data EventStream

instance S.Accept EventStream where
  contentType _ = "text" // "event-stream" /: ("charset", "utf-8")

-- | Recommended headers for Server-Sent Events.
type RecommendedEventSourceHeaders (a :: Type) = S.Headers '[S.Header "X-Accel-Buffering" Text, S.Header "Cache-Control" Text] a

-- | Add the recommended headers for Server-Sent Events to the response.
recommendedEventSourceHeaders :: a -> RecommendedEventSourceHeaders a
recommendedEventSourceHeaders = S.addHeader @"X-Accel-Buffering" "no" . S.addHeader @"Cache-Control" "no-store"

-- | A framing strategy for Server-Sent Events.
data ServerEventFraming

-- | Frames the server events by joining chunks with a newline.
instance S.FramingRender ServerEventFraming where
  framingRender _ f = fmap (\x -> f x <> "\n")
