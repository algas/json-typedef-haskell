{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (Enum)
import Data.JTD.Base
-- import Data.JTD.Schema
import Data.Aeson
import Data.Aeson.Types

main :: IO ()
main = do
  print (decode "{\"schema\": {},\"instance\": null,\"errors\": []}" :: Maybe JTD)
  json <- decodeJSON "./validation.json"
  print json
  -- print (decode "{\"foo\":\"bar\"}" :: Maybe Value)
  -- print $ encode emptyObject
  -- let done = decode "{\"name\":\"foo\",\"isAdmin\":true}" :: Maybe Properties
  -- print done
  -- print $ encode $ Properties "hoge" False (Just "fuga")
  -- let discriminator = decode "{\"eventType\":\"USER_CREATED\",\"id\":\"hoge\"}" :: Maybe Discriminator
  -- print discriminator
