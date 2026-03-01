# Coming from servant's built-in SSE

Servant 0.20.3.0 introduced SSE support in `Servant.API.ServerSentEvents`.
This guide shows equivalent code side by side so you can see how the two
libraries approach the same problems.

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

## Sending events (server side)

**servant built-in:**

Event encoding is determined by `EventKind` at the type level — `'JsonEvent`
serialises via `ToJSON`, `'RawEvent` sends raw bytes.

**servant-event-stream:**

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

**servant built-in:**

Use `ServerSentEvents'` with a custom method:

```haskell
type MyApi = "chat" :> ReqBody '[JSON] ChatRequest
                    :> ServerSentEvents' '[POST] 200 'JsonEvent ChatEvent
```

**servant-event-stream:**

Use `PostServerSentEvents`:

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
