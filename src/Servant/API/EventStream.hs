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
  ServerSentEvents,
  EventStream,
  EventSource,
  EventSourceHdr,
  eventSource,
  ServerEvent (..),
  ToServerEvent (..),
)
where

import Control.Lens
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as C8
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif
import Data.Kind (Type)
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

data ServerEvent = ServerEvent
  { eventType :: Maybe LBS.ByteString
  , eventId :: Maybe LBS.ByteString
  , eventData :: LBS.ByteString
  }
  deriving (Show, Eq, Generic)

class ToServerEvent a where
  toServerEvent :: a -> ServerEvent

{- | A ServerSentEvents endpoint emits an event stream using the format described at
  <https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#event_stream_format>
-}
data ServerSentEvents (a :: Type)
  deriving (Typeable, Generic)

instance HasLink (ServerSentEvents a) where
  type MkLink (ServerSentEvents a) r = r
  toLink toA _ = toA

instance (ToServerEvent a) => HasServer (ServerSentEvents a) context where
  type ServerT (ServerSentEvents a) m = ServerT (StreamGet ServerEventFraming EventStream (EventSourceHdr a)) m
  route Proxy =
    route
      (Proxy :: Proxy (StreamGet ServerEventFraming EventStream (EventSourceHdr a)))
  hoistServerWithContext Proxy =
    hoistServerWithContext
      (Proxy :: Proxy (StreamGet ServerEventFraming EventStream (EventSourceHdr a)))

-- | a helper instance for <https://hackage.haskell.org/package/servant-foreign-0.15.3/docs/Servant-Foreign.html servant-foreign>
instance
  (HasForeignType lang ftype (EventSourceHdr a)) =>
  HasForeign lang ftype (ServerSentEvents a)
  where
  type Foreign ftype (ServerSentEvents a) = Req ftype

  foreignFor lang Proxy Proxy req =
    req
      & reqFuncName . _FunctionName %~ ("stream" :)
      & reqMethod .~ method
      & reqReturnType ?~ retType
   where
    retType = typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy (EventSourceHdr a))
    method = reflectMethod (Proxy :: Proxy 'GET)

{- | A type representation of an event stream. It's responsible for setting proper content-type
  and buffering headers, as well as for providing parser implementations for the streams.
  Read more on <https://docs.servant.dev/en/stable/tutorial/Server.html#streaming-endpoints Servant Streaming Docs>
-}
data EventStream

instance Accept EventStream where
  contentType _ = "text" // "event-stream" /: ("charset", "utf-8")

type EventSource a = SourceIO a

{- | This is mostly to guide reverse-proxies like
  <https://www.nginx.com/resources/wiki/start/topics/examples/x-accel/#x-accel-buffering nginx>
-}
type EventSourceHdr (a :: Type) = Headers '[Header "X-Accel-Buffering" Text, Header "Cache-Control" Text] (EventSource a)

{- | See details at
  https://hackage.haskell.org/package/wai-extra-3.1.6/docs/Network-Wai-EventSource-EventStream.html#v:eventToBuilder
-}
instance (ToServerEvent a) => MimeRender EventStream a where
  mimeRender _ = encodeServerEvent . toServerEvent

instance ToServerEvent ServerEvent where
  toServerEvent = id

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

eventSource :: EventSource a -> EventSourceHdr a
eventSource = addHeader @"X-Accel-Buffering" "no" . addHeader @"Cache-Control" "no-store"

data ServerEventFraming

instance FramingRender ServerEventFraming where
  framingRender _ f = fmap (\x -> f x <> "\n")