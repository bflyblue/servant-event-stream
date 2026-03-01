# Upgrading from 0.3 to 0.4

## Breaking changes

### `ServerEvent` has two new fields

`eventComment` and `eventRetry` were added to support SSE comment and retry
directives. Code that pattern-matches on the `ServerEvent` constructor needs
the new fields:

```haskell
-- 0.3
ServerEvent (Just "update") (Just "1") "payload"

-- 0.4
ServerEvent (Just "update") (Just "1") "payload" Nothing Nothing
```

The `serverEvent` convenience constructor preserves the old 3-argument shape
and fills in the new fields as `Nothing`:

```haskell
-- works in both 0.3 and 0.4
serverEvent (Just "update") (Just "1") "payload"
```

If you were constructing `ServerEvent` values directly, switching to
`serverEvent` (or `dataEvent` for data-only events) is the easiest migration
path.

### Content type changed from `text/event-stream; charset=utf-8` to `text/event-stream`

The `Accept` instance no longer includes `charset=utf-8`. This matches the
[WHATWG SSE spec](https://html.spec.whatwg.org/multipage/server-sent-events.html),
which defines the MIME type without parameters. If you have client code that
checks the content type with the charset suffix, it will need updating.

## New exports

These are additive — existing code that doesn't use them is unaffected.

### Event constructors

| Function | Purpose |
|---|---|
| `serverEvent` | 3-argument constructor (type, id, data) — matches the old `ServerEvent` shape |
| `dataEvent` | Data-only event, no type or id |
| `commentEvent` | Comment line, useful as a keepalive heartbeat |
| `retryEvent` | Sets the client's reconnection delay |

### Receiving events

| Export | Purpose |
|---|---|
| `FromServerEvent` | Typeclass to parse a `ServerEvent` into a domain type |
| `decodeServerEvent` | Decode a single event block from its wire format |

### Client support

`HasClient` instances are provided for both `ServerSentEvents` and
`PostServerSentEvents`, so servant-client can consume SSE endpoints
directly. No extra setup needed — the instances are available as soon as
you import `Servant.API.EventStream`.

### POST endpoints

`PostServerSentEvents` works like `ServerSentEvents` but for POST
endpoints where the client sends a request body and receives a streamed
response (e.g. OpenAI chat completions).

### JSON helpers

| Export | Purpose |
|---|---|
| `jsonEvent` | Construct a `ServerEvent` with a JSON-encoded data payload |
| `jsonData` | Decode JSON from a `ServerEvent`'s data field |
| `JsonData` | `DerivingVia` newtype — derive `ToServerEvent` and `FromServerEvent` from `ToJSON`/`FromJSON` |

### Other

| Export | Purpose |
|---|---|
| `encodeServerEvent` | Encode a `ServerEvent` to its wire format (previously internal) |
| `ServerEventFraming` | SSE framing type (previously internal, needed for `StreamPost` endpoints) |

## New dependencies

- **aeson** — used by `JsonData`, `jsonEvent`, and `jsonData`.
- **servant-client-core** — used by the `HasClient` instances.

Both were already common transitive dependencies in most servant
applications.

## Behavioural changes

These are spec-compliance fixes. They shouldn't affect well-formed event
producers, but are worth noting:

- **Empty data is now emitted** as `data: \n` instead of being silently
  dropped. Previously, an event with empty `eventData` produced no output,
  which meant clients never saw the event.
- **Field sanitization**: CR and LF are stripped from `eventType`, `eventId`,
  and `eventComment` to prevent malformed multi-line output. NULL bytes are
  stripped from `eventId` per the WHATWG spec.
- **UTF-8 BOM**: a leading byte-order mark is stripped from SSE input, per the
  WHATWG spec. This helps when consuming streams from servers that prepend a
  BOM (common in .NET environments).
