module Servant.EventStream
  ( ServerSentEvents
  , EventStream
  , EventSource
  , eventSource
  )
where

import Data.Binary.Builder                  (toLazyByteString)
import Network.HTTP.Media                   ((//), (/:))
import Network.Wai.EventSource              (ServerEvent(..))
import Network.Wai.EventSource.EventStream  (eventToBuilder)
import Pipes
import Servant.API
import Servant.Pipes                        (pipesToSourceIO)

type ServerSentEvents = StreamGet NoFraming EventStream EventSource

data EventStream

instance Accept EventStream where
  contentType _ = "text" // "event-stream" /: ("charset", "utf-8")

type EventSource = SourceIO ServerEvent

instance MimeRender EventStream ServerEvent where
  mimeRender _ = maybe "" toLazyByteString . eventToBuilder

eventSource :: Proxy X () () ServerEvent IO () -> EventSource
eventSource prod = pipesToSourceIO (prod >-> yieldUntilClose)
 where
  yieldUntilClose = do
    e <- await
    case e of
      CloseEvent -> return ()
      _ -> yield e >> yieldUntilClose