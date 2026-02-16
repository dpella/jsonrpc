{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Maybe (isNothing)
import GHC.Generics (Generic)
import JSONRPC
import Test.Hspec
import Prelude hiding (error, id)
import Prelude qualified

-- ---------------------------------------------------------------------------
-- Example types for DerivingVia tests
-- ---------------------------------------------------------------------------

data PingRequest = PingRequest
    { id :: RequestId
    , params :: Maybe Value
    }
    deriving stock (Show, Eq, Generic)
    deriving (ToJSON, FromJSON) via ViaJSONRPCRequest PingRequest

instance IsJSONRPCRequest PingRequest where
    requestMethod _ = "ping"

data InitNotification = InitNotification
    { params :: Maybe Value
    }
    deriving stock (Show, Eq, Generic)
    deriving (ToJSON, FromJSON) via ViaJSONRPCNotification InitNotification

instance IsJSONRPCNotification InitNotification where
    notificationsMethod _ = "notifications/initialized"

-- | Helper to extract an Object from a Value, failing on non-objects.
asObject :: Value -> KM.KeyMap Value
asObject (Object o) = o
asObject _ = Prelude.error "expected object"

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
    describe "JSONRPC" $ do
        describe "Constants" $ do
            it "rPC_VERSION is \"2.0\"" $
                rPC_VERSION `shouldBe` "2.0"

        describe "Error codes" $ do
            it "pARSE_ERROR is -32700" $
                pARSE_ERROR `shouldBe` (-32700)
            it "iNVALID_REQUEST is -32600" $
                iNVALID_REQUEST `shouldBe` (-32600)
            it "mETHOD_NOT_FOUND is -32601" $
                mETHOD_NOT_FOUND `shouldBe` (-32601)
            it "iNVALID_PARAMS is -32602" $
                iNVALID_PARAMS `shouldBe` (-32602)
            it "iNTERNAL_ERROR is -32603" $
                iNTERNAL_ERROR `shouldBe` (-32603)

        describe "RequestId" $ do
            it "round-trips a numeric id" $ do
                let rid = RequestId (Number 42)
                decode (encode rid) `shouldBe` Just rid

            it "round-trips a string id" $ do
                let rid = RequestId (String "abc-123")
                decode (encode rid) `shouldBe` Just rid

            it "round-trips a null id" $ do
                let rid = RequestId Null
                decode (encode rid) `shouldBe` Just rid

        describe "JSONRPCErrorInfo" $ do
            it "serialises errorData as \"data\" (spec compliance)" $ do
                let info = JSONRPCErrorInfo (-32601) "Method not found" (Just (String "details"))
                    obj = asObject (toJSON info)
                KM.member "data" obj `shouldBe` True
                KM.member "errorData" obj `shouldBe` False

            it "omits \"data\" when errorData is Nothing" $ do
                let info = JSONRPCErrorInfo (-32601) "Method not found" Nothing
                    obj = asObject (toJSON info)
                KM.member "data" obj `shouldBe` False

            it "round-trips with data" $ do
                let info = JSONRPCErrorInfo (-32700) "Parse error" (Just (String "unexpected"))
                decode (encode info) `shouldBe` Just info

            it "round-trips without data" $ do
                let info = JSONRPCErrorInfo (-32600) "Invalid Request" Nothing
                decode (encode info) `shouldBe` Just info

            it "parses JSON with \"data\" key into errorData field" $ do
                let json = "{\"code\":-32601,\"message\":\"Method not found\",\"data\":\"extra\"}"
                case decode json :: Maybe JSONRPCErrorInfo of
                    Just info -> errorData info `shouldBe` Just (String "extra")
                    Nothing -> expectationFailure "Failed to parse JSONRPCErrorInfo"

            it "parses JSON without \"data\" key as Nothing" $ do
                let json = "{\"code\":-32601,\"message\":\"Method not found\"}"
                case decode json :: Maybe JSONRPCErrorInfo of
                    Just info -> isNothing (errorData info) `shouldBe` True
                    Nothing -> expectationFailure "Failed to parse JSONRPCErrorInfo"

        describe "JSONRPCRequest" $ do
            it "round-trips a request with params" $ do
                let req = JSONRPCRequest "2.0" (RequestId (Number 1)) "test" (object ["x" .= (1 :: Int)])
                decode (encode req) `shouldBe` Just req

            it "defaults missing params to Null" $ do
                let json = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}"
                case decode json :: Maybe JSONRPCRequest of
                    Just (JSONRPCRequest _ _ _ p) -> p `shouldBe` Null
                    Nothing -> expectationFailure "Failed to parse JSONRPCRequest"

            it "accepts explicit null params" $ do
                let json = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\",\"params\":null}"
                case decode json :: Maybe JSONRPCRequest of
                    Just (JSONRPCRequest _ _ _ p) -> p `shouldBe` Null
                    Nothing -> expectationFailure "Failed to parse JSONRPCRequest"

        describe "JSONRPCResponse" $ do
            it "round-trips a response" $ do
                let resp = JSONRPCResponse "2.0" (RequestId (Number 1)) (String "ok")
                decode (encode resp) `shouldBe` Just resp

        describe "JSONRPCError" $ do
            it "round-trips an error" $ do
                let err = JSONRPCError "2.0" (RequestId (Number 1)) (JSONRPCErrorInfo (-32601) "Not found" Nothing)
                decode (encode err) `shouldBe` Just err

            it "round-trips an error with data" $ do
                let err = JSONRPCError "2.0" (RequestId Null) (JSONRPCErrorInfo pARSE_ERROR "Parse error" (Just (String "detail")))
                decode (encode err) `shouldBe` Just err

        describe "JSONRPCNotification" $ do
            it "round-trips a notification with params" $ do
                let notif = JSONRPCNotification "2.0" "update" (object ["key" .= ("val" :: String)])
                decode (encode notif) `shouldBe` Just notif

            it "omits params when Null in serialised JSON" $ do
                let notif = JSONRPCNotification "2.0" "update" Null
                    obj = asObject (toJSON notif)
                KM.member "params" obj `shouldBe` False

            it "defaults missing params to Null on parse" $ do
                let json = "{\"jsonrpc\":\"2.0\",\"method\":\"update\"}"
                case decode json :: Maybe JSONRPCNotification of
                    Just (JSONRPCNotification _ _ p) -> p `shouldBe` Null
                    Nothing -> expectationFailure "Failed to parse JSONRPCNotification"

        describe "JSONRPCMessage" $ do
            it "parses a request as RequestMessage" $ do
                let json = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"foo\",\"params\":{}}"
                case decode json :: Maybe JSONRPCMessage of
                    Just (RequestMessage _) -> pure ()
                    other -> expectationFailure $ "Expected RequestMessage, got: " <> show other

            it "parses a response as ResponseMessage" $ do
                let json = "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":42}"
                case decode json :: Maybe JSONRPCMessage of
                    Just (ResponseMessage _) -> pure ()
                    other -> expectationFailure $ "Expected ResponseMessage, got: " <> show other

            it "parses an error as ErrorMessage" $ do
                let json = "{\"jsonrpc\":\"2.0\",\"id\":1,\"error\":{\"code\":-32601,\"message\":\"not found\"}}"
                case decode json :: Maybe JSONRPCMessage of
                    Just (ErrorMessage _) -> pure ()
                    other -> expectationFailure $ "Expected ErrorMessage, got: " <> show other

            it "parses a notification as NotificationMessage" $ do
                let json = "{\"jsonrpc\":\"2.0\",\"method\":\"update\"}"
                case decode json :: Maybe JSONRPCMessage of
                    Just (NotificationMessage _) -> pure ()
                    other -> expectationFailure $ "Expected NotificationMessage, got: " <> show other

            it "round-trips all message types" $ do
                let msgs =
                        [ RequestMessage $ JSONRPCRequest "2.0" (RequestId (Number 1)) "test" Null
                        , ResponseMessage $ JSONRPCResponse "2.0" (RequestId (Number 2)) (String "ok")
                        , ErrorMessage $ JSONRPCError "2.0" (RequestId (Number 3)) (JSONRPCErrorInfo (-32600) "Bad" Nothing)
                        , NotificationMessage $ JSONRPCNotification "2.0" "ping" Null
                        ]
                mapM_ (\m -> decode (encode m) `shouldBe` Just m) msgs

        describe "ViaJSONRPCRequest (DerivingVia)" $ do
            it "serialises a typed request with correct method" $ do
                let req = PingRequest (RequestId (Number 1)) Nothing
                    obj = asObject (toJSON req)
                KM.lookup "method" obj `shouldBe` Just (String "ping")
                KM.lookup "jsonrpc" obj `shouldBe` Just (String "2.0")

            it "round-trips a typed request" $ do
                let req = PingRequest (RequestId (Number 7)) (Just (object ["foo" .= True]))
                decode (encode req) `shouldBe` Just req

            it "rejects a request with wrong method" $ do
                let json = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"wrong\",\"params\":null}"
                (decode json :: Maybe PingRequest) `shouldBe` Nothing

        describe "ViaJSONRPCNotification (DerivingVia)" $ do
            it "serialises a typed notification with correct method" $ do
                let notif = InitNotification Nothing
                    obj = asObject (toJSON notif)
                KM.lookup "method" obj `shouldBe` Just (String "notifications/initialized")
                KM.lookup "jsonrpc" obj `shouldBe` Just (String "2.0")

            it "round-trips a typed notification" $ do
                let notif = InitNotification (Just (object ["meta" .= (1 :: Int)]))
                decode (encode notif) `shouldBe` Just notif

            it "rejects a notification with wrong method" $ do
                let json = "{\"jsonrpc\":\"2.0\",\"method\":\"wrong\",\"params\":null}"
                (decode json :: Maybe InitNotification) `shouldBe` Nothing
