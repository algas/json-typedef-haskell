{-# LANGUAGE OverloadedStrings #-}
module Data.JTD.Validator where

import Data.Aeson
import Data.JTD.Base

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

_validateJTD :: JTDSchema -> Value -> [JTDError] 
_validateJTD schema jtdInstance
  | schema == defaultJTDSchema = []
  | otherwise = []

validateJTD :: JTD -> Bool
validateJTD (JTD schema inst errors) = _validateJTD schema inst == errors
