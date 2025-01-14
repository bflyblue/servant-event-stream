{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aeson
import Data.Aeson.Text
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import Servant.API.ContentTypes
import Servant.API.EventStream
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)

data Order = Order
  { orderId :: !Text
  , orderDescription :: !Text
  , orderAmount :: !Int
  }
  deriving (Show, Eq)

instance ToJSON Order where
  toJSON Order{..} =
    object
      [ "orderId" .= orderId
      , "orderDescription" .= orderDescription
      , "orderAmount" .= orderAmount
      ]

instance FromJSON Order where
  parseJSON =
    withObject "Order" $ \o -> do
      orderId <- o .: "orderId"
      orderDescription <- o .: "orderDescription"
      orderAmount <- o .: "orderAmount"
      return Order{..}

arbitraryText :: Gen Text
arbitraryText = Text.pack <$> arbitrary

instance Arbitrary Order where
  arbitrary = do
    orderId <- arbitraryText
    orderDescription <- arbitraryText
    orderAmount <- arbitrary
    return Order{..}

instance ToServerEvent Order where
  toServerEvent order =
    ServerEvent
      { eventType = Just "order"
      , eventId = Nothing
      , eventData = LazyText.toStrict $ encodeToLazyText order
      }

instance FromServerEvent Order where
  fromServerEvent ServerEvent{..} = do
    case eventType of
      Just "order" ->
        case eitherDecodeStrictText eventData of
          Right order -> Right order
          Left err -> Left $ "Invalid event data: " <> err
      _ -> Left "Invalid event type"

exampleOrder :: Order
exampleOrder = Order "123" "Example order" 100

-- Unit tests
basicUnitTests :: TestTree
basicUnitTests =
  testGroup
    "Basic"
    [ testCase "trivial toServerEvent" $
        toServerEvent exampleOrder
          @=? ServerEvent
            { eventType = Just "order"
            , eventId = Nothing
            , eventData = "{\"orderAmount\":100,\"orderDescription\":\"Example order\",\"orderId\":\"123\"}"
            }
    , testCase "trivial fromServerEvent" $
        fromServerEvent
          ServerEvent
            { eventType = Just "order"
            , eventId = Nothing
            , eventData = "{\"orderAmount\":100,\"orderDescription\":\"Example order\",\"orderId\":\"123\"}"
            }
          @=? Right exampleOrder
    , testCase "trivial mimeUnrender" $
        mimeUnrender
          (Proxy :: Proxy EventStream)
          ( newlines
              [ ":This is a comment"
              , "event:order"
              , "data:{\"orderAmount\":100,\"orderDescription\":\"Example order\",\"orderId\":\"123\"}"
              , ""
              ]
          )
          @=? Right exampleOrder
    , testCase "mixed end of lines" $
        mimeUnrender
          (Proxy :: Proxy EventStream)
          ":This is a comment\n\revent:order\r\ndata:{\"orderAmount\":100,\"orderDescription\":\"Example order\",\"orderId\":\"123\"}\n\n"
          @=? Right exampleOrder
    , testCase "space after colon" $
        mimeUnrender
          (Proxy :: Proxy EventStream)
          ( newlines
              [ ":This is a comment"
              , "event: order"
              , "data: {\"orderAmount\":100,\"orderDescription\":\"Example order\",\"orderId\":\"123\"}"
              , ""
              ]
          )
          @=? Right exampleOrder
    , testCase "multiple spaces after colon" $
        mimeUnrender
          @EventStream
          @Order
          (Proxy :: Proxy EventStream)
          ( newlines
              [ ":This is a comment"
              , "event:   order"
              , "data:   {\"orderAmount\":100,\"orderDescription\":\"Example order\",\"orderId\":\"123\"}"
              , ""
              ]
          )
          @=? Left "Invalid event type"
    ]

newlines :: [LBS.ByteString] -> LBS.ByteString
newlines = LBS.intercalate "\n"

-- Properties
basicProperties :: TestTree
basicProperties =
  testGroup
    "Properties"
    [ testProperty "toServerEvent . fromServerEvent = id" prop_roundtrip
    ]
 where
  prop_roundtrip :: Order -> Bool
  prop_roundtrip order =
    Right order == fromServerEvent (toServerEvent order)

main :: IO ()
main = do
  defaultMain tests
 where
  tests =
    testGroup
      "Tests"
      [ basicUnitTests
      , basicProperties
      ]
