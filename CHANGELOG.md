# Revision history for servant-event-stream

## 0.4.0.0 -- 2026-02-27

* **Breaking**: Add `eventComment` and `eventRetry` fields to `ServerEvent`.
  Existing code using the `ServerEvent` constructor must add the new fields,
  e.g. `ServerEvent typ eid dat Nothing Nothing`.
* **Breaking**: Drop `charset=utf-8` from the `Accept` instance. The content
  type is now `text/event-stream` (no parameters), matching the WHATWG spec.
* **New dependency**: `aeson` (for JSON helpers).
* **New dependency**: `servant-client-core` (for `HasClient` instances).
* Add `serverEvent` convenience constructor matching the old 3-field API for easy migration.
* Add `dataEvent` convenience constructor for simple data-only events.
* Add `commentEvent` convenience constructor for heartbeat keepalives.
* Add `retryEvent` convenience constructor for setting client reconnection delay.
* Export `encodeServerEvent` for direct use outside of Servant.
* Add `FromServerEvent` typeclass and `decodeServerEvent` for parsing SSE events.
* Add `MimeUnrender EventStream` and `FramingUnrender ServerEventFraming` instances
  for consuming SSE streams.
* Add `HasClient` instances for `ServerSentEvents` and `PostServerSentEvents`,
  enabling client-side SSE consumption via `servant-client`.
* Add `PostServerSentEvents` combinator for POST SSE endpoints (e.g. OpenAI's
  streaming API), with `HasServer`, `HasClient`, and `HasForeign` instances.
* Add `JsonData` newtype for deriving `ToServerEvent` and `FromServerEvent` via
  JSON encoding using `DerivingVia`.
* Add `jsonEvent` helper to construct events with JSON-encoded data payloads.
* Add `jsonData` helper to decode JSON from an event's data field.
* Strip leading UTF-8 BOM from SSE input, per the WHATWG spec.
* Export `ServerEventFraming` (needed for `StreamPost` SSE endpoints).
* Always emit at least one `data:` field, even when `eventData` is empty.
  Previously, empty data produced no output and the event was silently
  dropped by clients.
* Sanitize `eventType`, `eventId`, and `eventComment` by stripping CR and LF
  characters to prevent malformed SSE output.
* Strip NULL characters from `eventId` (the SSE spec silently ignores ids
  containing NULL).

## 0.3.2.1 -- 2026-02-27

* Add guard space after colon in SSE field encoding. Without it, field values
  starting with a space lost their leading space because the SSE spec strips
  exactly one leading space from field values. (Issue #16)
* Add unit tests for `encodeServerEvent`.

## 0.3.2.0 -- 2026-02-23

* Support GHC 8.6 through 9.12 (base 4.12 to 4.21).
* Remove redundant `Typeable` deriving to fix `-Wderiving-typeable` warning on GHC 9.12+.
* Add CI test matrix for GHC 8.6, 8.10, 9.2, 9.8, 9.12.

## 0.3.1.1 -- 2026-02-23

* Constrain base bounds in test suite to match library.

## 0.3.1.0 -- 2025-08-21

* Qualify `servant` imports so that `ServerSentEvents` added in `servant-0.20.3.0` does not conflict with ours. (Issue #12)

## 0.3.0.0 -- 2024-09-05

* Breaking changes to the API.

    Event streams are implemented using servant's 'Stream' endpoint. You should
    provide a handler that returns a stream of events that implements
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

## 0.2.1.0 -- 2021-04-21

* Import `Data.Semigroup` for base < 4.11.0

## 0.2.0.0 -- 2021-04-20

* `Servant.EventStream` was moved to `Servant.API.EventStream` to adhere existing [upstream layout](https://hackage.haskell.org/package/servant-0.18.2/docs/Servant-API-Stream.html).

## 0.1.0.0 -- 2018-04-30

* First version. Released on an unsuspecting world.
