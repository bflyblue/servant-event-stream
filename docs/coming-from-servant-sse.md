# Coming from servant's built-in SSE

Servant 0.20.3.0 introduced SSE support in `Servant.API.ServerSentEvents`.
The built-in module focuses on client-side consumption (`HasClient` instances),
while this library provides both server and client support. This guide shows
how the concepts map between the two.

## Defining an API

**servant built-in:**

```haskell
import Servant.API.ServerSentEvents (ServerSentEvents, EventKind(..))

type MyApi = "events" :> ServerSentEvents 'JsonEvent NewsItem
```

**servant-event-stream:**

```haskell
import Servant.API.EventStream (ServerSentEvents)

type MyApi = "events" :> ServerSentEvents (SourceIO NewsItem)
```

The stream type (`SourceIO`) is explicit in the API, which is the same pattern
servant uses for other streaming endpoints.

## Serving events

The built-in `ServerSentEvents` does not yet include `HasServer` instances, so
it can't be used directly in servant server handlers. This library provides
`HasServer` instances for both `ServerSentEvents` and `PostServerSentEvents`.

You write a `ToServerEvent` instance that maps each constructor to a
`ServerEvent`. This gives you control over the `event:`, `id:`, and `data:`
fields per constructor:

```haskell
data NewsItem = NewsItem { title :: Text, body :: Text }
  deriving (Generic, ToJSON, FromJSON)

-- Option 1: DerivingVia for simple JSON-in-data events
deriving via JsonData NewsItem instance ToServerEvent NewsItem

-- Option 2: manual instance for full control
instance ToServerEvent NewsItem where
  toServerEvent item = jsonEvent (Just "news") Nothing item
```

## Consuming events (client side)

Both libraries provide `HasClient` instances for consuming SSE streams.

**servant built-in:**

The `EventKind` determines how events are decoded — `'JsonEvent` uses
`FromJSON` automatically.

**servant-event-stream:**

You write a `FromServerEvent` instance. For simple JSON types, `DerivingVia`
does the work:

```haskell
-- Option 1: DerivingVia (mirrors the ToServerEvent approach)
deriving via JsonData NewsItem instance FromServerEvent NewsItem

-- Option 2: manual instance with event type dispatch
instance FromServerEvent NewsItem where
  fromServerEvent ev = case eventType ev of
    Just "news" -> jsonData ev
    _           -> Left "unknown event"
```

## POST endpoints

The built-in `ServerSentEvents'` accepts a method parameter, so you can write
`ServerSentEvents' '[POST] 200 'JsonEvent ChatEvent` in your API type. However,
since there are no `HasServer` instances, this currently only works on the
client side.

This library provides `PostServerSentEvents` with full server and client
support:

```haskell
type MyApi = "chat" :> ReqBody '[JSON] ChatRequest
                    :> PostServerSentEvents (SourceIO ChatEvent)
```

## Full example

Here's a complete streaming endpoint using servant-event-stream:

```haskell
{-# LANGUAGE DataKinds, DeriveGeneric, DerivingVia, OverloadedStrings, TypeOperators #-}

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Servant
import Servant.API.EventStream

data NewsItem = NewsItem { title :: Text, body :: Text }
  deriving (Show, Generic)
  deriving (ToJSON, FromJSON)
  deriving (ToServerEvent, FromServerEvent) via JsonData NewsItem

type MyApi = "news" :> ServerSentEvents (SourceIO NewsItem)

server :: Server MyApi
server = pure $ source
  [ NewsItem "Breaking" "Something happened"
  , NewsItem "Update"   "More details emerged"
  ]
```
