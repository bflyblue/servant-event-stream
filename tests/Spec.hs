{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Lazy as LBS
import Data.Proxy (Proxy (..))
import Servant.API.ContentTypes (mimeRender)
import Servant.API.EventStream
import Test.Hspec

render :: ServerEvent -> LBS.ByteString
render = mimeRender (Proxy :: Proxy EventStream)

main :: IO ()
main = hspec $ do
  describe "MimeRender EventStream ServerEvent" $ do
    it "encodes data-only event" $
      render (ServerEvent Nothing Nothing "hello")
        `shouldBe` "data: hello\n"

    it "encodes event with all fields" $
      render (ServerEvent (Just "update") (Just "1") "payload")
        `shouldBe` "event: update\nid: 1\ndata: payload\n"

    it "encodes multi-line data as multiple data fields" $
      render (ServerEvent Nothing Nothing "line1\nline2\nline3")
        `shouldBe` "data: line1\ndata: line2\ndata: line3\n"

    it "preserves leading space in data value" $
      render (ServerEvent Nothing Nothing " How")
        `shouldBe` "data:  How\n"

    it "preserves leading space in event type" $
      render (ServerEvent (Just " custom") Nothing "x")
        `shouldBe` "event:  custom\ndata: x\n"

    it "preserves leading space in event id" $
      render (ServerEvent Nothing (Just " 42") "x")
        `shouldBe` "id:  42\ndata: x\n"

    it "encodes empty data as empty output" $
      render (ServerEvent Nothing Nothing "")
        `shouldBe` ""

    it "strips CR characters from data" $
      render (ServerEvent Nothing Nothing "hello\r\nworld")
        `shouldBe` "data: hello\ndata: world\n"

    it "omits event field when Nothing" $
      render (ServerEvent Nothing (Just "1") "x")
        `shouldBe` "id: 1\ndata: x\n"

    it "omits id field when Nothing" $
      render (ServerEvent (Just "ping") Nothing "x")
        `shouldBe` "event: ping\ndata: x\n"
