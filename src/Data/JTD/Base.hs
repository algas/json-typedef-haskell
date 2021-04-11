{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.JTD.Base where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Int (Int8, Int16, Int32)
import Data.Word (Word8, Word16, Word32)
import Data.Scientific (floatingOrInteger)
import Data.Vector (Vector)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (isJust)

import GHC.Generics

data JTDError = JTDError {
    instancePath :: [String]
  , schemaPath :: [String]
} deriving (Show, Generic)
instance FromJSON JTDError

-- data JTDRawInstanceType
--   = JTDRawEmpty
--   | JTDRawBoolean Bool
--   | JTDRawFloat Double 
--   | JTDRawInteger Integer
--   | JTDRawString Text
--   | JTDRawArray (Vector JTDRawInstanceType)
--   deriving (Show)
-- instance FromJSON JTDRawInstanceType where
--   parseJSON Null = return JTDRawEmpty
--   parseJSON (Bool b) = return $ JTDRawBoolean b
--   parseJSON (Number n) = case floatingOrInteger n of
--     Left f -> return $ JTDRawFloat f
--     Right i -> return $ JTDRawInteger i
--   parseJSON (String t) = return $ JTDRawString t
--   parseJSON (Array a) = return $ JTDRawArray a

data JTDSchema = JTDSchema {
    jtdtype :: Maybe Text
  , jtdenum :: Maybe [Text]
  , jtdelements :: Maybe JTDSchema
  , jtdproperties :: Maybe (HashMap Text JTDSchema)
  , jtdoptionalProperties :: Maybe (HashMap Text JTDSchema)
  , jtdadditionalProperties :: Maybe Bool
  , jtdvalues :: Maybe JTDSchema
  , jtddiscriminator :: Maybe Text
  , jtdmapping :: Maybe JTDSchema
  , jtdref :: Maybe Text
  , jtdnullable :: Maybe Bool
  , jtdemetadata :: Maybe Value
} deriving (Show, Generic)
instance FromJSON JTDSchema where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = drop 3 }


-- data JTDProperties = JTDProperties {
--     jtdproperties :: HashMap Text JTDSchema
--   , jtdoptionalProperties :: Maybe (HashMap Text JTDSchema)
--   , jtdadditionalProperties :: Maybe Bool
-- } deriving (Show, Generic)
-- instance FromJSON JTDProperties where
--   parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = drop 3 }

-- data JTDDiscriminator = JTDDiscriminator {
--     jtddiscriminator :: Text
--   , jtdmapping :: JTDSchema
-- } deriving (Show, Generic)
-- instance FromJSON JTDDiscriminator where
--   parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = drop 3 }

-- data JTDSchema
--   = JTDSchemaType Text
--   | JTDSchemaEnum [Text]
--   | JTDSchemaElements JTDSchema
--   | JTDSchemaProperties JTDProperties
--   | JTDSchemaValues JTDSchema
--   | JTDSchemaDiscriminator JTDDiscriminator
--   | JTDSchemaRef Text
--   deriving (Show)
-- instance FromJSON JTDSchema where
--   parseJSON = withObject "Object" $ \o -> do
--     nullable <- o .:? "nullable"
--     metadata <- o .:? "metadata"
--     _type <- o .:? "type"
--     enum <- o .:? "enum"
--     elements <- o .:? "elements"
--     properties <- o .:? "properties"
--     optionalProperties <- o .:? "optionalProperties"
--     additionalProperties <- o .:? "additionalProperties"
--     values <- o .:? "values"
--     discriminator <- o .:? "discriminator"
--     mapping <- o .:? "mapping"
--     ref <- o .:? "ref"
--     case (_type, enum, elements, properties, values, discriminator, ref) of
--       (Just t,_,_,_,_,_,_) -> return $ JTDSchemaType t
--       _ -> fail "object is not one of JTD values" 


data JTD = JTD {
    jtdschema :: JTDSchema
  , jtdinstance :: Value
  , jtderrors :: [JTDError]
} deriving (Show, Generic)

instance FromJSON JTD where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = drop 3 }
