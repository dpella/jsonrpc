# Changelog

## 0.2.0

### Added
- Initial release.
- Core JSON-RPC 2.0 types: `JSONRPCRequest`, `JSONRPCResponse`, `JSONRPCError`,
  `JSONRPCNotification`, `JSONRPCMessage`, `JSONRPCErrorInfo`, `RequestId`.
- Type classes for deriving JSON-RPC serialisation: `IsJSONRPCRequest`,
  `IsJSONRPCNotification` with `ViaJSONRPCRequest` / `ViaJSONRPCNotification`
  newtype wrappers for `DerivingVia`.
- Standard JSON-RPC 2.0 error codes: `pARSE_ERROR`, `iNVALID_REQUEST`,
  `mETHOD_NOT_FOUND`, `iNVALID_PARAMS`, `iNTERNAL_ERROR`.
- `rPC_VERSION` constant (`"2.0"`).
- `EmptyParams` placeholder type for methods with no parameters.
