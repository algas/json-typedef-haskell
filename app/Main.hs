{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (Enum)
import Data.JTD.Schema
import Data.Aeson

main :: IO ()
main = do
  let done = decode "{\"name\":\"foo\",\"isAdmin\":true}" :: Maybe Properties
  print done
  print $ encode $ Properties "hoge" False Nothing
  let discriminator = decode "{\"eventType\":\"USER_CREATED\",\"id\":\"hoge\"}" :: Maybe Discriminator
  print discriminator
