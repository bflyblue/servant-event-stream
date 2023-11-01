{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Network.HTTP.Client.TLS
import Servant
import Servant.API.EventStream
import Servant.Client.Streaming
import Servant.Types.SourceT as S
import qualified Data.Text as T


type NumEvents  = Int
type PostmanSSE = "server-events" :> Capture "numberOfEvents" Int
                                  :> StreamGet NoFraming EventStream EventSource

postmanBaseUrl :: BaseUrl
postmanBaseUrl = BaseUrl Https "postman-echo.com" 443 ""

api :: Proxy PostmanSSE
api = Proxy

postmanSSEGet :: NumEvents -> ClientM EventSource
postmanSSEGet = client api

-- | Performs a request to the free service over at Postman
-- https://postman-echo.com/server-events/:numberOfEvents
-- Requests 5 events, and print them on screen as they arrive.
-- When executed, prints something like this:
--
-- "ServerEvent<info,1,Keep listening to server updates>"
-- "ServerEvent<notification,2,Update request received>"
-- "ServerEvent<info,3,Keep listening to server updates>"
-- "ServerEvent<info,4,Keep listening to server updates>"
-- "ServerEvent<message,5,Message at time 2023-11-01T07:29:33+00:00>"
--
main :: IO ()
main = do
  mgr <- newTlsManager
  let env             = mkClientEnv mgr postmanBaseUrl
      streamingClient = postmanSSEGet 5
  withClientM streamingClient env $ \case
    Left err  -> putStrLn $ show err
    Right src -> S.unSourceT src processEvent
  where
    processEvent S.Stop         = pure ()
    processEvent (S.Error err)  = putStrLn err
    processEvent (S.Skip s)     = processEvent s
    processEvent (S.Effect ms)  = ms >>= processEvent
    processEvent (S.Yield se s) = putStrLn (T.unpack $ renderServerEvent se) >> processEvent s
