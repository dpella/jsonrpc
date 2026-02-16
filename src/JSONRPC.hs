{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : JSONRPC
Description : JSON-RPC 2.0 types and type classes
Copyright   : (c) 2026 DPella AB
License     : MPL-2.0
Maintainer  : matti@dpella.io, lobo@dpella.io
Stability   : experimental
Portability : GHC

An implementation of the [JSON-RPC 2.0](https://www.jsonrpc.org/specification)
protocol types for Haskell, with Aeson serialisation.

= Core types

The four message kinds defined by the specification:

* 'JSONRPCRequest'      — a method call expecting a response
* 'JSONRPCResponse'     — a successful response
* 'JSONRPCError'        — an error response
* 'JSONRPCNotification' — a one-way message (no response)

'JSONRPCMessage' is a tagged union of all four, useful when reading messages
off the wire without knowing the kind in advance.

= Deriving support

'IsJSONRPCRequest' and 'IsJSONRPCNotification' let you define strongly-typed
request\/notification types and derive their JSON-RPC encoding via
@DerivingVia@:

@
data PingRequest = PingRequest
    { id :: RequestId, params :: Maybe () }
    deriving (ToJSON, FromJSON) via ViaJSONRPCRequest PingRequest

instance IsJSONRPCRequest PingRequest where
    requestMethod _ = \"ping\"
@
-}
module JSONRPC (
    -- * Request identifiers
    RequestId (..),

    -- * Type classes
    IsJSONRPCRequest (..),
    IsJSONRPCNotification (..),

    -- * Deriving-via wrappers
    ViaJSONRPCRequest (..),
    ViaJSONRPCNotification (..),

    -- * JSON-RPC message types
    JSONRPCRequest (..),
    JSONRPCResponse (..),
    JSONRPCError (..),
    JSONRPCNotification (..),
    JSONRPCMessage (..),
    JSONRPCErrorInfo (..),

    -- * Helpers
    EmptyParams,

    -- * Standard JSON-RPC 2.0 error codes
    pARSE_ERROR,
    iNVALID_REQUEST,
    mETHOD_NOT_FOUND,
    iNVALID_PARAMS,
    iNTERNAL_ERROR,

    -- * Constants
    rPC_VERSION,
) where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Data (Proxy (..), Typeable, typeRep)
import Data.Kind (Type)
import Data.Text (Text)
import GHC.Generics
import GHC.Records (HasField (getField))
import GHC.TypeError (ErrorMessage (..), TypeError)

-- ---------------------------------------------------------------------------
-- RequestId
-- ---------------------------------------------------------------------------

{- | A uniquely identifying ID for a request in JSON-RPC.

The JSON-RPC 2.0 specification requires that each request has a unique
identifier.  The ID can be a string, number, or null value.  Numbers
SHOULD NOT contain fractional parts.
-}
newtype RequestId = RequestId Value
    deriving stock (Show, Eq)
    deriving newtype (ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

{- | JSON-RPC protocol version string (@\"2.0\"@).

Every request, response, and notification includes a @jsonrpc@ field
that MUST be exactly this value.
-}
rPC_VERSION :: Text
rPC_VERSION = "2.0"

-- ---------------------------------------------------------------------------
-- EmptyParams
-- ---------------------------------------------------------------------------

{- | Placeholder type for methods that take no parameters.

'EmptyParams' serialises to JSON @null@ and always fails to parse
(it is only used as a type-level marker; the 'FromJSON' instance for
'JSONRPCRequest' defaults missing params to 'Null').
-}
data EmptyParams
    deriving stock (Generic)

instance Show EmptyParams where
    show :: EmptyParams -> String
    show _ = show Aeson.Null

instance Eq EmptyParams where
    (==) :: EmptyParams -> EmptyParams -> Bool
    _ == _ = True

instance ToJSON EmptyParams where
    toJSON :: EmptyParams -> Value
    toJSON _ = Aeson.Null

instance FromJSON EmptyParams where
    parseJSON _ = fail "EmptyParams can only be used as a placeholder for empty params"

-- ---------------------------------------------------------------------------
-- JSON-RPC error information
-- ---------------------------------------------------------------------------

{- | Structured error information inside a JSON-RPC error response.

@
{ "code": -32601, "message": "Method not found", "data": ... }
@

The @code@ and @message@ fields are mandatory; @data@ is optional and may
contain any additional information about the error.
-}
data JSONRPCErrorInfo = JSONRPCErrorInfo
    { code :: Int
    -- ^ A number indicating the error type that occurred.
    , message :: Text
    -- ^ A short description of the error.
    , errorData :: Maybe Value
    -- ^ Additional information about the error (serialised as @\"data\"@ in JSON).
    }
    deriving stock (Show, Eq, Generic)

{- | Custom 'ToJSON' that maps the Haskell field @errorData@ to the JSON key @\"data\"@
as required by the JSON-RPC 2.0 specification.
-}
instance ToJSON JSONRPCErrorInfo where
    toJSON (JSONRPCErrorInfo c m d) =
        object $
            [ "code" .= c
            , "message" .= m
            ]
                <> maybe [] (\v -> ["data" .= v]) d

-- | Custom 'FromJSON' that reads the JSON key @\"data\"@ into the Haskell field @errorData@.
instance FromJSON JSONRPCErrorInfo where
    parseJSON = withObject "JSONRPCErrorInfo" $ \o ->
        JSONRPCErrorInfo
            <$> o .: "code"
            <*> o .: "message"
            <*> o .:? "data"

-- ---------------------------------------------------------------------------
-- Core message types
-- ---------------------------------------------------------------------------

{- | A JSON-RPC request that expects a response.

@
{ "jsonrpc": "2.0", "id": 1, "method": "foo", "params": { ... } }
@
-}
data JSONRPCRequest = JSONRPCRequest
    { jsonrpc :: Text
    -- ^ Always @\"2.0\"@.
    , id :: RequestId
    -- ^ A unique identifier for the request.
    , method :: Text
    -- ^ The method to invoke.
    , params :: Value
    -- ^ The method parameters (defaults to 'Null' if omitted).
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON)

{- | Custom 'FromJSON' instance that treats a missing @params@ key as 'Null'.
JSON-RPC 2.0 allows the @params@ field to be omitted.
-}
instance FromJSON JSONRPCRequest where
    parseJSON = withObject "JSONRPCRequest" $ \o ->
        JSONRPCRequest
            <$> o .: "jsonrpc"
            <*> o .: "id"
            <*> o .: "method"
            <*> o .:? "params" .!= Null

{- | A successful (non-error) response to a request.

@
{ "jsonrpc": "2.0", "id": 1, "result": { ... } }
@
-}
data JSONRPCResponse = JSONRPCResponse
    { jsonrpc :: Text
    -- ^ Always @\"2.0\"@.
    , id :: RequestId
    -- ^ Must match the @id@ of the corresponding request.
    , result :: Value
    -- ^ The result of the method invocation.
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

{- | A response to a request that indicates an error occurred.

@
{ "jsonrpc": "2.0", "id": 1, "error": { "code": -32601, "message": "..." } }
@
-}
data JSONRPCError = JSONRPCError
    { jsonrpc :: Text
    -- ^ Always @\"2.0\"@.
    , id :: RequestId
    -- ^ Must match the @id@ of the corresponding request (or 'Null' for parse errors).
    , error :: JSONRPCErrorInfo
    -- ^ Structured error information.
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

{- | A notification which does not expect a response.

@
{ "jsonrpc": "2.0", "method": "update", "params": { ... } }
@

Notifications have no @id@ field.  The server MUST NOT reply to a notification.
-}
data JSONRPCNotification = JSONRPCNotification
    { jsonrpc :: Text
    -- ^ Always @\"2.0\"@.
    , method :: Text
    -- ^ The method name.
    , params :: Value
    -- ^ The method parameters (omitted from JSON when 'Null').
    }
    deriving stock (Show, Eq, Generic)

-- | Custom 'ToJSON' instance that omits @params@ when it is 'Null'.
instance ToJSON JSONRPCNotification where
    toJSON (JSONRPCNotification j m Null) =
        object ["jsonrpc" .= j, "method" .= m]
    toJSON (JSONRPCNotification j m p) =
        object ["jsonrpc" .= j, "method" .= m, "params" .= p]

{- | Custom 'FromJSON' instance that treats a missing @params@ key as 'Null'.
JSON-RPC 2.0 allows notifications to omit the @params@ field entirely.
-}
instance FromJSON JSONRPCNotification where
    parseJSON = withObject "JSONRPCNotification" $ \o ->
        JSONRPCNotification
            <$> o .: "jsonrpc"
            <*> o .: "method"
            <*> o .:? "params" .!= Null

{- | Any valid JSON-RPC message that can be decoded off the wire.

Useful when reading from a transport where the message kind is unknown.
-}
data JSONRPCMessage
    = RequestMessage JSONRPCRequest
    | ResponseMessage JSONRPCResponse
    | ErrorMessage JSONRPCError
    | NotificationMessage JSONRPCNotification
    deriving stock (Show, Eq, Generic)

instance ToJSON JSONRPCMessage where
    toJSON (RequestMessage r) = toJSON r
    toJSON (ResponseMessage r) = toJSON r
    toJSON (ErrorMessage e) = toJSON e
    toJSON (NotificationMessage n) = toJSON n

instance FromJSON JSONRPCMessage where
    parseJSON v =
        (RequestMessage <$> parseJSON v)
            <|> (ResponseMessage <$> parseJSON v)
            <|> (ErrorMessage <$> parseJSON v)
            <|> (NotificationMessage <$> parseJSON v)

-- ---------------------------------------------------------------------------
-- IsJSONRPCRequest
-- ---------------------------------------------------------------------------

-- | Type family to extract the type of the @params@ field from a generic representation.
type family RequestParamType (rep :: Type -> Type) :: Type where
    RequestParamType (D1 _ (C1 _ (S1 _ (K1 _ RequestId) :*: S1 _ (K1 _ p)))) = p
    RequestParamType (D1 ('MetaData nm _ _ _) _) =
        TypeError
            ( ('Text "Error when defining IsJSONRPCRequest for " :<>: 'Text nm :<>: 'Text ":")
                :$$: 'Text "The request datatype must be a record with fields 'id' and 'params'"
            )

{- | Type class for Haskell types that represent a specific JSON-RPC request method.

Instances define the method name via 'requestMethod'.  Serialisation and
deserialisation are provided by default using GHC Generics: the type must
be a two-field record with @id :: RequestId@ and @params :: SomeParamsType@.

Use 'ViaJSONRPCRequest' for @DerivingVia@-based 'ToJSON'\/'FromJSON' instances.
-}
class
    ( HasField "id" a RequestId
    , HasField "params" a (RequestParams a)
    , Typeable a
    , ToJSON (RequestParams a)
    , FromJSON (RequestParams a)
    ) =>
    IsJSONRPCRequest a
    where
    -- | The type of the @params@ field.
    type RequestParams a

    type RequestParams a = RequestParamType (Rep a)

    -- | The JSON-RPC method name (e.g. @\"initialize\"@, @\"tools\/list\"@).
    requestMethod :: Proxy a -> Text

    -- | Convert a typed request to a raw 'JSONRPCRequest'.
    toJSONRPCRequest :: a -> JSONRPCRequest
    toJSONRPCRequest (req :: a) =
        JSONRPCRequest rPC_VERSION (getField @"id" req) (requestMethod (Proxy @a)) (toJSON (getField @"params" req))

    -- | Parse a raw 'JSONRPCRequest' into a typed request.
    fromJSONRPCRequest :: JSONRPCRequest -> Either String a
    default fromJSONRPCRequest ::
        ( Generic a
        , Rep a ~ D1 c0 (C1 i0 (S1 s0 (K1 i1 RequestId) :*: S1 s1 (K1 i30 (RequestParams a))))
        ) =>
        JSONRPCRequest ->
        Either String a
    fromJSONRPCRequest JSONRPCRequest{id = req_id, params = req_params} =
        case fromJSON @(RequestParams a) req_params of
            Success p -> Right $ to $ M1 (M1 (M1 (K1 req_id) :*: M1 (K1 p)))
            Aeson.Error err -> Left err

-- | Newtype wrapper for deriving 'ToJSON' and 'FromJSON' via 'IsJSONRPCRequest'.
newtype ViaJSONRPCRequest a = ViaJSONRPCRequest {unViaJSONRPCRequest :: a}

instance (IsJSONRPCRequest a) => ToJSON (ViaJSONRPCRequest a) where
    toJSON = toJSON . toJSONRPCRequest . unViaJSONRPCRequest

instance (IsJSONRPCRequest a) => FromJSON (ViaJSONRPCRequest a) where
    parseJSON = withObject (show $ typeRep (Proxy @a)) $ \o -> do
        m <- o .: "method"
        req_id <- o .: "id"
        if m == requestMethod (Proxy @a)
            then do
                p <- o .:? "params" .!= Null
                case fromJSONRPCRequest (JSONRPCRequest rPC_VERSION req_id m p) of
                    Right r -> return (ViaJSONRPCRequest r)
                    Left err -> fail $ "Failed to parse params for " <> show (typeRep (Proxy @a)) <> ": " <> err
            else fail $ "Expected method '" <> show (requestMethod (Proxy @a)) <> "'"

-- ---------------------------------------------------------------------------
-- IsJSONRPCNotification
-- ---------------------------------------------------------------------------

-- | Type family to extract the type of the @params@ field from a notification's generic representation.
type family NotificationParamType (rep :: Type -> Type) :: Type where
    NotificationParamType (D1 _ (C1 _ (S1 _ (K1 _ p)))) = p
    NotificationParamType (D1 ('MetaData nm _ _ _) _) =
        TypeError
            ( ('Text "Error when defining IsJSONRPCNotification for " :<>: 'Text nm :<>: 'Text ":")
                :$$: 'Text "The notification datatype must be a record with a single field 'params'"
            )

{- | Type class for Haskell types that represent a specific JSON-RPC notification method.

Similar to 'IsJSONRPCRequest' but for notifications (no @id@ field).
Use 'ViaJSONRPCNotification' for @DerivingVia@-based instances.
-}
class
    ( HasField "params" a (NotificationParams a)
    , Typeable a
    , ToJSON (NotificationParams a)
    , FromJSON (NotificationParams a)
    ) =>
    IsJSONRPCNotification a
    where
    -- | The type of the @params@ field.
    type NotificationParams a

    type NotificationParams a = NotificationParamType (Rep a)

    -- | The JSON-RPC method name.
    notificationsMethod :: Proxy a -> Text

    -- | Convert a typed notification to a raw 'JSONRPCNotification'.
    toJSONRPCNotification :: a -> JSONRPCNotification
    toJSONRPCNotification (req :: a) =
        JSONRPCNotification rPC_VERSION (notificationsMethod (Proxy @a)) (toJSON (getField @"params" req))

    -- | Parse a raw 'JSONRPCNotification' into a typed notification.
    fromJSONRPCNotification :: JSONRPCNotification -> Either String a
    default fromJSONRPCNotification ::
        ( Generic a
        , Rep a ~ D1 c0 (C1 i0 (S1 s1 (K1 i30 (NotificationParams a))))
        ) =>
        JSONRPCNotification ->
        Either String a
    fromJSONRPCNotification JSONRPCNotification{params = notification_params} =
        case fromJSON @(NotificationParams a) notification_params of
            Success p -> Right $ to $ M1 (M1 (M1 (K1 p)))
            Aeson.Error err -> Left err

-- | Newtype wrapper for deriving 'ToJSON' and 'FromJSON' via 'IsJSONRPCNotification'.
newtype ViaJSONRPCNotification a = ViaJSONRPCNotification {unViaJSONRPCNotification :: a}

instance (IsJSONRPCNotification a) => ToJSON (ViaJSONRPCNotification a) where
    toJSON = toJSON . toJSONRPCNotification . unViaJSONRPCNotification

instance (IsJSONRPCNotification a) => FromJSON (ViaJSONRPCNotification a) where
    parseJSON = withObject (show $ typeRep (Proxy @a)) $ \o -> do
        m <- o .: "method"
        if m == notificationsMethod (Proxy @a)
            then do
                p <- o .:? "params" .!= Null
                case fromJSONRPCNotification (JSONRPCNotification rPC_VERSION m p) of
                    Right r -> return (ViaJSONRPCNotification r)
                    Left err -> fail $ "Failed to parse params for " <> show (typeRep (Proxy @a)) <> ": " <> err
            else fail $ "Expected method '" <> show (notificationsMethod (Proxy @a)) <> "'"

-- ---------------------------------------------------------------------------
-- Standard JSON-RPC 2.0 error codes
-- ---------------------------------------------------------------------------

{- | Parse error (@-32700@).

Invalid JSON was received by the server.
-}
pARSE_ERROR :: Int
pARSE_ERROR = -32700

{- | Invalid request (@-32600@).

The JSON sent is not a valid Request object.
-}
iNVALID_REQUEST :: Int
iNVALID_REQUEST = -32600

{- | Method not found (@-32601@).

The method does not exist or is not available.
-}
mETHOD_NOT_FOUND :: Int
mETHOD_NOT_FOUND = -32601

{- | Invalid params (@-32602@).

Invalid method parameter(s).
-}
iNVALID_PARAMS :: Int
iNVALID_PARAMS = -32602

{- | Internal error (@-32603@).

Internal JSON-RPC error.
-}
iNTERNAL_ERROR :: Int
iNTERNAL_ERROR = -32603
