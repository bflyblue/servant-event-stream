# servant-event-stream

[Server-Sent Events](https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events) (SSE) support for [Servant](https://github.com/haskell-servant/). Stream events from your server to connected clients, or consume third-party SSE streams (such as [OpenAI](https://platform.openai.com/docs/api-reference/streaming)) from Haskell.

## Quick start

Define a domain type and a `ToServerEvent` instance. Use `jsonEvent` to JSON-encode the data payload, or `serverEvent` for raw bytestrings:

```haskell
data ChatEvent
  = ContentDelta Text
  | ContentDone Text
  | ChatDone

type MyApi = "chat" :> ServerSentEvents (SourceIO ChatEvent)

instance ToServerEvent ChatEvent where
  toServerEvent (ContentDelta d) = jsonEvent (Just "response.content.delta") Nothing d
  toServerEvent (ContentDone t) = jsonEvent (Just "response.content.done") Nothing t
  toServerEvent ChatDone = jsonEvent (Just "response.done") Nothing ()

server :: Server MyApi
server = pure $ source
  [ContentDelta "Hel", ContentDelta "lo!", ContentDone "Hello!", ChatDone]
```

For POST endpoints (e.g. OpenAI chat completions), use `PostServerSentEvents`:

```haskell
type MyApi = "chat" :> ReqBody '[JSON] ChatRequest
                    :> PostServerSentEvents (SourceIO ChatEvent)
```

To consume an SSE stream, provide a `FromServerEvent` instance. Use `jsonData` to decode JSON from the data field:

```haskell
instance FromServerEvent ChatEvent where
  fromServerEvent ev = case eventType ev of
    Just "response.content.delta" -> ContentDelta <$> jsonData ev
    Just "response.content.done"  -> ContentDone <$> jsonData ev
    Just "response.done"          -> Right ChatDone
    _                             -> Left "unknown event"
```

For types that serialise entirely as JSON (no event type dispatch), use `DerivingVia`:

```haskell
data Temperature = Temperature { celsius :: Double }
  deriving (Generic, ToJSON, FromJSON)
  deriving (ToServerEvent, FromServerEvent) via JsonData Temperature
```

See the [Haddock documentation](https://hackage.haskell.org/package/servant-event-stream/docs/Servant-API-EventStream.html) for the full API reference, or the [upgrading guide](docs/upgrading-from-0.3.md) if you're coming from 0.3.

## Coming from servant's built-in SSE

Servant 0.20.3.0 introduced its own `ServerSentEvents` type in
`Servant.API.ServerSentEvents`. If you'd like to try this library instead,
here's how the concepts map:

| servant built-in | servant-event-stream |
|---|---|
| `Servant.API.ServerSentEvents` | `Servant.API.EventStream` |
| `ServerSentEvents 'JsonEvent MyEvent` | `ServerSentEvents (SourceIO MyEvent)` |
| `EventKind` (`RawEvent` / `JsonEvent`) | `ToServerEvent` / `FromServerEvent` typeclasses |

This library takes a typeclass approach: `ToServerEvent` and `FromServerEvent`
control how your domain types map to SSE fields, giving you access to event
names, ids, comments, and retry directives in a single `ServerEvent` record.
It also provides `PostServerSentEvents` for POST endpoints, and JSON helpers
(`jsonEvent`, `jsonData`, `JsonData`) for APIs that encode payloads as JSON.

The built-in module focuses on client-side consumption (`HasClient` instances),
while this library provides both `HasServer` and `HasClient` instances.
See the [side-by-side examples](docs/coming-from-servant-sse.md) for a more
detailed walkthrough.

## Development

Uses [Nix](https://nixos.org) flakes for the development environment.

```bash
nix develop       # enter dev shell
nix build         # full build
nix flake check   # CI-equivalent check
cabal test        # run tests (inside dev shell)
```
