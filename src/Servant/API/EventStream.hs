{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.API.EventStream (
  ServerSentEvents,
  EventStream,
  EventSource,
  EventSourceHdr,
  eventSource,
)
where

import Control.Lens
import Data.Binary.Builder (toLazyByteString)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Media (
  (//),
  (/:),
 )
import Network.Wai.EventSource (ServerEvent (..))
import Network.Wai.EventSource.EventStream (
  eventToBuilder,
 )
import Servant
import Servant.Foreign
import Servant.Foreign.Internal (_FunctionName)

newtype ServerSentEvents
  = ServerSentEvents (StreamGet NoFraming EventStream EventSourceHdr)
  deriving (Generic, HasLink)

instance HasServer ServerSentEvents context where
  type ServerT ServerSentEvents m = ServerT (StreamGet NoFraming EventStream EventSourceHdr) m
  route Proxy =
    route
      (Proxy :: Proxy (StreamGet NoFraming EventStream EventSourceHdr))
  hoistServerWithContext Proxy =
    hoistServerWithContext
      (Proxy :: Proxy (StreamGet NoFraming EventStream EventSourceHdr))

-- | a helper instance for <https://hackage.haskell.org/package/servant-foreign-0.15.3/docs/Servant-Foreign.html servant-foreign>
instance
  (HasForeignType lang ftype EventSourceHdr) =>
  HasForeign lang ftype ServerSentEvents
  where
  type Foreign ftype ServerSentEvents = Req ftype

  foreignFor lang Proxy Proxy req =
    req
      & reqFuncName . _FunctionName %~ ("stream" :)
      & reqMethod .~ method
      & reqReturnType ?~ retType
   where
    retType = typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy EventSourceHdr)
    method = reflectMethod (Proxy :: Proxy 'GET)

{- | A type representation of an event stream. It's responsible for setting proper content-type
  and buffering headers, as well as for providing parser implementations for the streams.
  Read more on <https://docs.servant.dev/en/stable/tutorial/Server.html#streaming-endpoints Servant Streaming Docs>
-}
data EventStream

instance Accept EventStream where
  contentType _ = "text" // "event-stream" /: ("charset", "utf-8")

type EventSource = SourceIO ServerEvent

{- | This is mostly to guide reverse-proxies like
  <https://www.nginx.com/resources/wiki/start/topics/examples/x-accel/#x-accel-buffering nginx>
-}
type EventSourceHdr = Headers '[Header "X-Accel-Buffering" Text] EventSource

{- | See details at
  https://hackage.haskell.org/package/wai-extra-3.1.6/docs/Network-Wai-EventSource-EventStream.html#v:eventToBuilder
-}
instance MimeRender EventStream ServerEvent where
  mimeRender _ = maybe "" toLazyByteString . eventToBuilder

eventSource :: EventSource -> EventSourceHdr
eventSource = addHeader "no"
