# Generating Haskell from JSON Typedef schemas

## Generated Haskell code

### Code generated from “Empty” schemas

```json
{}
```

```hs
data Empty
```

### Code generated from “Ref” schemas

```json
{
  "definitions": {
    "example": { "type": "string" }
  },
  "ref": "example"
}
```

```hs
type Example = String
type Ref = Example
```

### Code generated from “Type” schemas

```json
{
  "properties": {
    "boolean": { "type": "boolean" },
    "string": { "type": "string" },
    "timestamp": { "type": "timestamp" },
    "float32": { "type": "float32" },
    "float64": { "type": "float64" },
    "int8": { "type": "int8" },
    "uint8": { "type": "uint8" },
    "int16": { "type": "int16" },
    "uint16": { "type": "uint16" },
    "int32": { "type": "int32" },
    "uint32": { "type": "uint32" }
  }
}
```

```hs
import Data.Time (UTCTime)
import Data.Int (Int8, Int16, Int32)
import Data.Word (Word8, Word16, Word32)

data Type = Type {
    boolean :: Bool
  , string :: String
  , timestamp :: UTCTime
  , float32 :: Float
  , float64 :: Double
  , int8 :: Int8
  , uint8 :: Word8
  , int16 :: Int16
  , uint16 :: Word16
  , int32 :: Int32
  , uint32 :: Word32
}
```

### Code generated from “Enum” schemas

```json
{
  "enum": ["PENDING", "IN_PROGRESS", "DONE"]
}
```

```hs
data Enum = Done | InProgress | Pending
```

### Code generated from “Elements” schemas

```json
{
  "elements": {
    "type": "string"
  }
}
```

```hs
type Elements = [String]
```

### Code generated from “Properties” schemas

```json
{
  "properties": {
    "name": { "type": "string" },
    "isAdmin": { "type": "boolean" }
  },
  "optionalProperties": {
    "middleName": { "type": "string" }
  },
  "additionalProperties": true
}
```

```hs
data Properties = Properties {
    name :: String
  , isAdmin :: Bool
  , middleName :: Maybe String
}
```

### Code generated from “Values” schemas

```json
{
  "values": {
    "type": "string"
  }
}
```

```hs
import Data.Map (Map)
type Values = Map String String
```

### Code generated from “Discriminator” schemas

```json
{
  "discriminator": "eventType",
  "mapping": {
    "USER_CREATED": {
      "properties": {
        "id": { "type": "string" }
      }
    },
    "USER_PAYMENT_PLAN_CHANGED": {
      "properties": {
        "id": { "type": "string" },
        "plan": { "enum": ["FREE", "PAID"] }
      }
    },
    "USER_DELETED": {
      "properties": {
        "id": { "type": "string" },
        "softDelete": { "type": "boolean" }
      }
    }
  }
}
```

```hs
data EventType = UserCreated | UserPaymentPlanChanged | UserDeleted
data DiscriminatorUserPaymentPlanChangedPlan = Free | Paid
data Discriminator = 
    DiscriminatorUserCreated {
      _id :: String
    }
  | DiscriminatorUserPaymentPlanChanged {
      _id :: String
    , softDelete: Bool
    }
  | DiscriminatorUserDeleted {
      _id :: String
    , plan :: DiscriminatorUserPaymentPlanChangedPlan
    }
```

