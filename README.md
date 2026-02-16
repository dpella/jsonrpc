# jsonrpc

[![Hackage](https://img.shields.io/hackage/v/jsonrpc.svg)](https://hackage.haskell.org/package/jsonrpc)

A lightweight Haskell implementation of
[JSON-RPC 2.0](https://www.jsonrpc.org/specification) protocol types with
[Aeson](https://hackage.haskell.org/package/aeson) serialisation.

## Features

- Core message types: requests, responses, notifications, and errors
- `IsJSONRPCRequest` / `IsJSONRPCNotification` type classes for automatic
  method dispatch via `DerivingVia`
- Standard error codes from the specification
- Spec-compliant JSON encoding (optional `params`, error `data` field, etc.)

## Quick start

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

import JSONRPC
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- Define your request type
data PingRequest = PingRequest
    { id :: RequestId
    , params :: Maybe ()
    }
    deriving stock (Show, Eq, Generic)
    deriving (ToJSON, FromJSON) via ViaJSONRPCRequest PingRequest

instance IsJSONRPCRequest PingRequest where
    requestMethod _ = "ping"
```

## License

[MPL-2.0](LICENSE)
