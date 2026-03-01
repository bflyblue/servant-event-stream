{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Proxy (Proxy (..))
import Servant.API.ContentTypes (mimeRender, mimeUnrender)
import Servant.API.EventStream
import Test.Hspec

-- A domain type mirroring the haddock usage example.
data VehicleEvent
  = KeyOn
  | KeyOff
  | Position Double Double
  deriving (Show, Eq)

instance ToServerEvent VehicleEvent where
  toServerEvent KeyOn              = serverEvent (Just "keyOn") Nothing ""
  toServerEvent KeyOff             = serverEvent (Just "keyOff") Nothing ""
  toServerEvent (Position lat lon) =
    serverEvent (Just "position") Nothing
      (C8.pack (show lat <> "," <> show lon))

instance FromServerEvent VehicleEvent where
  fromServerEvent ev = case eventType ev of
    Just "keyOn"    -> Right KeyOn
    Just "keyOff"   -> Right KeyOff
    Just "position" -> parsePosition (eventData ev)
    _               -> Left "unknown vehicle event"

parsePosition :: LBS.ByteString -> Either String VehicleEvent
parsePosition bs =
  case break (== ',') (C8.unpack bs) of
    (latStr, ',':lonStr) ->
      case (reads latStr, reads lonStr) of
        ([(lat, "")], [(lon, "")]) -> Right (Position lat lon)
        _ -> Left "invalid coordinates"
    _ -> Left "invalid position format"

render :: ServerEvent -> LBS.ByteString
render = mimeRender (Proxy :: Proxy EventStream)

decode :: LBS.ByteString -> Either String ServerEvent
decode = mimeUnrender (Proxy :: Proxy EventStream)

main :: IO ()
main = hspec $ do
  describe "MimeRender EventStream ServerEvent" $ do
    it "encodes data-only event" $
      render (dataEvent "hello")
        `shouldBe` "data: hello\n"

    it "encodes event with all fields" $
      render (ServerEvent (Just "update") (Just "1") "payload" (Just "note") (Just 5000))
        `shouldBe` ": note\nretry: 5000\nevent: update\nid: 1\ndata: payload\n"

    it "encodes multi-line data as multiple data fields" $
      render (serverEvent Nothing Nothing "line1\nline2\nline3")
        `shouldBe` "data: line1\ndata: line2\ndata: line3\n"

    it "preserves leading space in data value" $
      render (serverEvent Nothing Nothing " How")
        `shouldBe` "data:  How\n"

    it "preserves leading space in event type" $
      render (serverEvent(Just " custom") Nothing "x")
        `shouldBe` "event:  custom\ndata: x\n"

    it "preserves leading space in event id" $
      render (serverEvent Nothing (Just " 42") "x")
        `shouldBe` "id:  42\ndata: x\n"

    it "encodes empty data as a single data field" $
      render (serverEvent Nothing Nothing "")
        `shouldBe` "data: \n"

    it "encodes empty data with event type" $
      render (serverEvent(Just "ping") Nothing "")
        `shouldBe` "event: ping\ndata: \n"

    it "handles trailing newline in data" $
      render (dataEvent "hello\n")
        `shouldBe` "data: hello\n"

    it "handles data that is only a newline" $
      render (dataEvent "\n")
        `shouldBe` "data: \n"

    it "handles data that is only CR" $
      render (dataEvent "\r")
        `shouldBe` "data: \n"

    it "strips CR characters from data" $
      render (serverEvent Nothing Nothing "hello\r\nworld")
        `shouldBe` "data: hello\ndata: world\n"

    it "emits empty event type" $
      render (serverEvent (Just "") Nothing "x")
        `shouldBe` "event: \ndata: x\n"

    it "emits empty event id" $
      render (serverEvent Nothing (Just "") "x")
        `shouldBe` "id: \ndata: x\n"

    it "omits event field when Nothing" $
      render (serverEvent Nothing (Just "1") "x")
        `shouldBe` "id: 1\ndata: x\n"

    it "omits id field when Nothing" $
      render (serverEvent(Just "ping") Nothing "x")
        `shouldBe` "event: ping\ndata: x\n"

  describe "comment support" $ do
    it "encodes a comment-only event" $
      render (commentEvent "keepalive")
        `shouldBe` ": keepalive\ndata: \n"

    it "encodes comment with data" $
      render (ServerEvent Nothing Nothing "hello" (Just "debug") Nothing)
        `shouldBe` ": debug\ndata: hello\n"

    it "encodes empty comment" $
      render (commentEvent "")
        `shouldBe` ": \ndata: \n"

    it "strips CR and LF from comment" $
      render (commentEvent "line1\r\nline2")
        `shouldBe` ": line1line2\ndata: \n"

  describe "retry support" $ do
    it "encodes a retry-only event" $
      render (retryEvent 3000)
        `shouldBe` "retry: 3000\ndata: \n"

    it "encodes retry zero" $
      render (retryEvent 0)
        `shouldBe` "retry: 0\ndata: \n"

    it "encodes retry with data" $
      render (ServerEvent Nothing Nothing "hello" Nothing (Just 1000))
        `shouldBe` "retry: 1000\ndata: hello\n"

  describe "field sanitization" $ do
    it "strips LF from event type" $
      render (serverEvent(Just "bad\ntype") Nothing "x")
        `shouldBe` "event: badtype\ndata: x\n"

    it "strips CR from event type" $
      render (serverEvent(Just "bad\rtype") Nothing "x")
        `shouldBe` "event: badtype\ndata: x\n"

    it "strips CRLF from event id" $
      render (serverEvent Nothing (Just "bad\r\nid") "x")
        `shouldBe` "id: badid\ndata: x\n"

    it "strips NULL from event id" $
      render (serverEvent Nothing (Just "abc\0def") "x")
        `shouldBe` "id: abcdef\ndata: x\n"

  describe "convenience constructors" $ do
    it "dataEvent is equivalent to serverEvent Nothing Nothing" $
      dataEvent "hello" `shouldBe` serverEvent Nothing Nothing "hello"

    it "commentEvent sets comment field" $
      commentEvent "hi" `shouldBe` ServerEvent Nothing Nothing "" (Just "hi") Nothing

    it "retryEvent sets retry field" $
      retryEvent 5000 `shouldBe` ServerEvent Nothing Nothing "" Nothing (Just 5000)

  describe "decodeServerEvent" $ do
    it "decodes data-only event" $
      decode "data: hello\n"
        `shouldBe` Right (dataEvent "hello")

    it "decodes event with all fields" $
      decode ": note\nretry: 5000\nevent: update\nid: 1\ndata: payload\n"
        `shouldBe` Right (ServerEvent (Just "update") (Just "1") "payload" (Just "note") (Just 5000))

    it "decodes multi-line data" $
      decode "data: line1\ndata: line2\ndata: line3\n"
        `shouldBe` Right (dataEvent "line1\nline2\nline3")

    it "strips exactly one leading space from values" $
      decode "data: hello\n"
        `shouldBe` Right (dataEvent "hello")

    it "handles no space after colon" $
      decode "data:hello\n"
        `shouldBe` Right (dataEvent "hello")

    it "preserves leading space beyond the first" $
      decode "data:  How\n"
        `shouldBe` Right (dataEvent " How")

    it "decodes comment" $
      decode ": keepalive\ndata: x\n"
        `shouldBe` Right (ServerEvent Nothing Nothing "x" (Just "keepalive") Nothing)

    it "decodes comment with space stripping" $
      decode ":keepalive\n"
        `shouldBe` Right (ServerEvent Nothing Nothing "" (Just "keepalive") Nothing)

    it "decodes retry" $
      decode "retry: 3000\ndata: x\n"
        `shouldBe` Right (ServerEvent Nothing Nothing "x" Nothing (Just 3000))

    it "ignores retry with non-digit value" $
      decode "retry: abc\ndata: x\n"
        `shouldBe` Right (dataEvent "x")

    it "ignores retry with empty value" $
      decode "retry:\ndata: x\n"
        `shouldBe` Right (dataEvent "x")

    it "ignores id containing NULL" $
      decode "id: abc\0def\ndata: x\n"
        `shouldBe` Right (dataEvent "x")

    it "decodes CRLF line endings" $
      decode "data: hello\r\ndata: world\r\n"
        `shouldBe` Right (dataEvent "hello\nworld")

    it "decodes empty data" $
      decode "data: \n"
        `shouldBe` Right (dataEvent "")

    it "decodes empty data without space" $
      decode "data:\n"
        `shouldBe` Right (dataEvent "")

    it "decodes event type" $
      decode "event: update\ndata: x\n"
        `shouldBe` Right (serverEvent (Just "update") Nothing "x")

    it "decodes event id" $
      decode "id: 42\ndata: x\n"
        `shouldBe` Right (serverEvent Nothing (Just "42") "x")

    it "ignores unknown fields" $
      decode "foo: bar\ndata: hello\n"
        `shouldBe` Right (dataEvent "hello")

    it "handles field name with no colon as empty value" $
      decode "data\n"
        `shouldBe` Right (dataEvent "")

    it "uses last comment when multiple present" $
      decode ": first\n: second\ndata: x\n"
        `shouldBe` Right (ServerEvent Nothing Nothing "x" (Just "second") Nothing)

    it "uses last event type when multiple present" $
      decode "event: a\nevent: b\ndata: x\n"
        `shouldBe` Right (serverEvent (Just "b") Nothing "x")

    it "decodes empty input as empty event" $
      decode ""
        `shouldBe` Right (ServerEvent Nothing Nothing "" Nothing Nothing)

  describe "roundtrip (encode then decode)" $ do
    it "roundtrips data-only event" $
      decode (render (dataEvent "hello"))
        `shouldBe` Right (dataEvent "hello")

    it "roundtrips event with type and id" $
      decode (render (serverEvent (Just "update") (Just "1") "payload"))
        `shouldBe` Right (serverEvent (Just "update") (Just "1") "payload")

    it "roundtrips event with all fields" $
      let e = ServerEvent (Just "update") (Just "1") "payload" (Just "note") (Just 5000)
      in decode (render e) `shouldBe` Right e

    it "roundtrips multi-line data" $
      decode (render (dataEvent "line1\nline2\nline3"))
        `shouldBe` Right (dataEvent "line1\nline2\nline3")

    it "roundtrips comment event" $
      decode (render (commentEvent "keepalive"))
        `shouldBe` Right (commentEvent "keepalive")

    it "roundtrips retry event" $
      decode (render (retryEvent 3000))
        `shouldBe` Right (retryEvent 3000)

    it "roundtrips empty data" $
      decode (render (dataEvent ""))
        `shouldBe` Right (dataEvent "")

    it "roundtrips event with leading space in data" $
      decode (render (dataEvent " hello"))
        `shouldBe` Right (dataEvent " hello")

  describe "custom type (ToServerEvent / FromServerEvent)" $ do
    let renderV :: VehicleEvent -> LBS.ByteString
        renderV = mimeRender (Proxy :: Proxy EventStream)
        decodeV :: LBS.ByteString -> Either String VehicleEvent
        decodeV = mimeUnrender (Proxy :: Proxy EventStream)

    it "encodes KeyOn" $
      renderV KeyOn
        `shouldBe` "event: keyOn\ndata: \n"

    it "encodes KeyOff" $
      renderV KeyOff
        `shouldBe` "event: keyOff\ndata: \n"

    it "encodes Position" $
      renderV (Position 51.5 (-0.5))
        `shouldBe` "event: position\ndata: 51.5,-0.5\n"

    it "decodes KeyOn" $
      decodeV "event: keyOn\ndata: \n"
        `shouldBe` Right KeyOn

    it "decodes KeyOff" $
      decodeV "event: keyOff\ndata: \n"
        `shouldBe` Right KeyOff

    it "decodes Position" $
      decodeV "event: position\ndata: 51.5,-0.5\n"
        `shouldBe` Right (Position 51.5 (-0.5))

    it "roundtrips KeyOn" $
      decodeV (renderV KeyOn) `shouldBe` Right KeyOn

    it "roundtrips KeyOff" $
      decodeV (renderV KeyOff) `shouldBe` Right KeyOff

    it "roundtrips Position" $
      decodeV (renderV (Position 51.5 (-0.5)))
        `shouldBe` Right (Position 51.5 (-0.5))

    it "rejects unknown event type" $
      decodeV "event: other\ndata: hello\n"
        `shouldBe` Left "unknown vehicle event"

    it "rejects missing event type" $
      decodeV "data: hello\n"
        `shouldBe` Left "unknown vehicle event"
