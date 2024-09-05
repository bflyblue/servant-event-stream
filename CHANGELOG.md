# Revision history for servant-event-stream

## 0.3.0.0 -- 2024-09-05

* Breaking changes to the API.

    Event streams are implemented using servant's 'Stream' endpoint. You should
    provide a handler that returns a stream of events that implements 'ToSourceIO'
    where events have a 'ToServerEvent' instance.

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
