{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.JTD.Schema where

import Prelude hiding (Enum)
import Data.Aeson
import Data.Aeson.Types
import Data.Map (Map)
import Data.Time (UTCTime)
import Data.Int (Int8, Int16, Int32)
import Data.Word (Word8, Word16, Word32)

type Empty = ()

type Example = String
type Ref = Example

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

data Enum = Done | InProgress | Pending
  deriving (Show)
instance FromJSON Enum where
  parseJSON = withText "Enum" $ \case
    "DONE" -> return Done
    "IN_PROGRESS" -> return InProgress
    "PENDING" -> return Pending
    _ -> fail "string is not one of known enum values"
instance ToJSON Enum where
  toJSON Done = "DONE"
  toJSON InProgress = "IN_PROGRESS"
  toJSON Pending = "PENDING"


data Properties = Properties {
    name :: String
  , isAdmin :: Bool
  , middleName :: Maybe String
} deriving (Show)
instance FromJSON Properties where
  parseJSON = withObject "Properties" $ \o -> do
    name <- o .: "name"
    isAdmin <- o .: "isAdmin"
    middleName <- o .:? "middleName"
    return Properties{..}
instance ToJSON Properties where
  toJSON Properties{..} = object [
    "name" .= name,
    "isAdmin" .= isAdmin,
    -- omit when middleName is Nothing
    "middleName" .= middleName ]


type Values = Map String String

data DiscriminatorUserPaymentPlanChangedPlan = Free | Paid
  deriving (Show)
instance FromJSON DiscriminatorUserPaymentPlanChangedPlan where
  parseJSON = withText "DiscriminatorUserPaymentPlanChangedPlan" $ \case
    "FREE" -> return Free
    "PAID" -> return Paid
    _ -> fail "string is not one of known enum values"
instance ToJSON DiscriminatorUserPaymentPlanChangedPlan where
  toJSON Free = "FREE"
  toJSON Paid = "PAID"

data Discriminator = 
    DiscriminatorUserCreated {
      userCreatedId :: String
    }
  | DiscriminatorUserPaymentPlanChanged {
      userPaymentPlanChangedId :: String
    , plan :: DiscriminatorUserPaymentPlanChangedPlan
    }
  | DiscriminatorUserDeleted {
      userDeletedId :: String
    , softDelete :: Bool
    }
  deriving (Show)

instance FromJSON Discriminator where
  parseJSON = withObject "Discriminator" $ \o -> do
    eventType <- o .: "eventType"
    case eventType of
      "USER_CREATED" -> DiscriminatorUserCreated <$> o .: "id"
      "USER_PAYMENT_PLAN_CHANGED" -> DiscriminatorUserPaymentPlanChanged <$> o .: "id" <*> o .: "plan"
      "USER_DELETED" -> DiscriminatorUserDeleted <$> o .: "id" <*> o .: "softDelete"
      _ -> fail ("unknown eventType" ++ eventType)

instance ToJSON Discriminator where
  toJSON DiscriminatorUserCreated{..} = object [
    "eventType" .= String "USER_CREATED",
    "id" .= userCreatedId ]
  toJSON DiscriminatorUserPaymentPlanChanged{..} = object [
    "eventType" .= String "USER_PAYMENT_PLAN_CHANGED",
    "id" .= userPaymentPlanChangedId,
    "plan" .= plan ]
  toJSON DiscriminatorUserDeleted{..} = object [
    "eventType" .= String "USER_DELETED",
    "id" .= userDeletedId,
    "softDelete" .= softDelete ]
