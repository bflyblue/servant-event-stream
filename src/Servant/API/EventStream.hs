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

module Servant.API.EventStream (
  ServerEvent (..),
  ToServerEvent (..),
  ServerSentEvents,
  EventStream,
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
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Network.HTTP.Media (
  (//),
  (/:),
 )
import Servant
import Servant.Foreign
import Servant.Foreign.Internal (_FunctionName)

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
  { eventType :: !(Maybe LBS.ByteString)
  -- ^ Optional field specifying the type of event. Can be used to distinguish between different kinds of events.
  , eventId :: !(Maybe LBS.ByteString)
  -- ^ Optional field providing an identifier for the event. Useful for clients to keep track of the last received event.
  , eventData :: !LBS.ByteString
  -- ^ The payload or content of the event. This is the main data sent to the client.
  }
  deriving (Show, Eq, Generic)

{- | This typeclass allows you to define custom event types that can be
  transformed into the 'ServerEvent' type, which is used to represent events in
  the Server-Sent Events (SSE) protocol.
-}
class ToServerEvent a where
  toServerEvent :: a -> ServerEvent

instance (ToServerEvent a) => MimeRender EventStream a where
  mimeRender _ = encodeServerEvent . toServerEvent

instance ToServerEvent ServerEvent where
  toServerEvent = id

{- | Event streams are implemented using servant's 'Stream' endpoint.
  You should provide a handler that returns a stream of events that implements
  'ToSourceIO' where events have a 'ToServerEvent' instance.

  Example:

  > type MyApi = "books" :> ServerSentEvents (SourceIO Book)
  >
  > instance ToServerEvent Book where
  >   toServerEvent book = ...
  >
  > server :: Server MyApi
  > server = streamBooks
  >   where streamBooks :: Handler (SourceIO Book)
  >         streamBooks = pure $ source [book1, ...]
-}
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

{- | This is mostly to guide reverse-proxies like
  <https://www.nginx.com/resources/wiki/start/topics/examples/x-accel/#x-accel-buffering nginx>

  Example:

  > type MyApi = "books" :> ServerSentEvents (RecommendedEventSourceHeaders (SourceIO Book))
  >
  > server :: Server MyApi
  > server = streamBooks
  >   where streamBooks :: Handler (RecommendedEventSourceHeaders (SourceIO Book))
  >         streamBooks = pure $ recommendedEventSourceHeaders $ source [book1, ...]
-}
type RecommendedEventSourceHeaders (a :: Type) = Headers '[Header "X-Accel-Buffering" Text, Header "Cache-Control" Text] a

-- | Add the recommended headers for Server-Sent Events to the response.
recommendedEventSourceHeaders :: a -> RecommendedEventSourceHeaders a
recommendedEventSourceHeaders = addHeader @"X-Accel-Buffering" "no" . addHeader @"Cache-Control" "no-store"

data ServerEventFraming

instance FramingRender ServerEventFraming where
  framingRender _ f = fmap (\x -> f x <> "\n")

{- 1. Field names must not contain LF, CR or COLON characters.
   2. Values must not contain LF or CR characters.
      Multple consecutive `data:` fields will be joined with LFs on the client.
-}
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
