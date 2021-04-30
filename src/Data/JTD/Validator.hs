{-# LANGUAGE OverloadedStrings #-}
module Data.JTD.Validator where

import Data.Aeson
import Data.JTD.Base
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import Data.Vector (Vector)
import Data.Maybe (isJust, fromJust)
import Data.Scientific (Scientific, toBoundedInteger, toRealFloat)
import Data.Int (Int8, Int16, Int32)
import Data.Word (Word8, Word16, Word32)
import Data.Either (isRight)

data JTDTypeValue = 
    JTDBoolean
  | JTDString
  | JTDTimestamp
  | JTDFloat32
  | JTDFloat64
  | JTDInt8
  | JTDUInt8
  | JTDInt16
  | JTDUInt16
  | JTDInt32
  | JTDUInt32
  deriving (Show, Eq)

data JTD
  = JTDEmpty
  | JTDType JTDTypeValue
  -- | JTDEnum [Text]
  | JTDElements (Vector JTD)
  | JTDProperties (HashMap Text JTD)
  | JTDValues JTD
  -- | JTDDiscriminator (HashMap Text JTD)
  | JTDNullable (Maybe JTD)
  deriving (Show, Eq)

defaultJTDSchema = JTDSchema {
    jtdtype = Nothing
  , jtdenum = Nothing
  , jtdelements = Nothing
  , jtdproperties = Nothing
  , jtdoptionalProperties = Nothing
  , jtdadditionalProperties = Nothing
  , jtdvalues = Nothing
  , jtddiscriminator = Nothing
  , jtdmapping = Nothing
  , jtdref = Nothing
  , jtdnullable = Nothing
  , jtdemetadata = Nothing
}

defaultJTDError = JTDError {
    instancePath = []
  , schemaPath = []
}


typeToJTD :: Text -> Maybe JTD
typeToJTD schema = case schema of
  "boolean" -> Just $ JTDType JTDBoolean
  "string" -> Just $ JTDType JTDString
  "timestamp" -> Just $ JTDType JTDTimestamp
  "float32" -> Just $ JTDType JTDFloat32
  "float64" -> Just $ JTDType JTDFloat64
  "int8" -> Just $ JTDType JTDInt8
  "uint8" -> Just $ JTDType JTDUInt8
  "int16" -> Just $ JTDType JTDInt16
  "uint16" -> Just $ JTDType JTDUInt16
  "int32" -> Just $ JTDType JTDInt32
  "uint32" -> Just $ JTDType JTDUInt32
  _ -> Nothing

schemaToJTD :: JTDSchema -> Maybe JTD
schemaToJTD schema@(JTDSchema _type enum elems props op ap vs disc mapping ref nullable meta)
  | schema == defaultJTDSchema = Just JTDEmpty
  | isJust _type = typeToJTD $ fromJust _type
  | otherwise = Nothing


_validateJTDType :: Value -> JTDTypeValue -> Bool
_validateJTDType v t = case (v, t) of
  (Bool _, JTDBoolean) -> True
  (String _, JTDString) -> True
  (String _, JTDTimestamp) -> True -- TODO: validate timestamp
  (Number num, JTDFloat32) -> let n = toRealFloat num in floatDigits n == 24
  (Number num, JTDFloat64) -> let n = toRealFloat num in floatDigits n == 53
  (Number num, JTDInt8) -> let n = toBoundedInteger num in isJust n && ((minBound :: Int8) <= fromJust n) && (fromJust n <= (maxBound :: Int8))
  (Number num, JTDUInt8) -> let n = toBoundedInteger num in isJust n && ((minBound :: Word8) <= fromJust n) && (fromJust n <= (maxBound :: Word8))
  (Number num, JTDInt16) -> let n = toBoundedInteger num in isJust n && ((minBound :: Int16) <= fromJust n) && (fromJust n <= (maxBound :: Int16))
  (Number num, JTDUInt16) -> let n = toBoundedInteger num in isJust n && ((minBound :: Word16) <= fromJust n) && (fromJust n <= (maxBound :: Word16))
  (Number num, JTDInt32) -> let n = toBoundedInteger num in isJust n && ((minBound :: Int32) <= fromJust n) && (fromJust n <= (maxBound :: Int32))
  (Number num, JTDUInt32) -> let n = toBoundedInteger num in isJust n && ((minBound :: Word32) <= fromJust n) && (fromJust n <= (maxBound :: Word32))
  _ -> False

validateJTDValue :: Value -> JTD -> Bool
validateJTDValue v jtd = case (v, jtd) of
  (_, JTDEmpty) -> True
  (v, JTDType t) -> _validateJTDType v t
  _ -> False


_validateJTD :: Value -> Maybe JTD -> [JTDError] 
_validateJTD jtdInstance jtd
  | isJust jtd = []
  | otherwise = []

validateJTD :: JTDValidation -> Bool
validateJTD (JTDValidation schema inst errors) = validateJTDValue inst (fromJust (schemaToJTD schema))
-- TODO: to check diff to errors 
