{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit
import Data.JTD.Base
import Data.JTD.Validator
import Data.HashMap.Strict (toList)

testJTD label jtd = TestCase $ assertBool label $ validateJTD jtd

main :: IO ()
main = do
  validationData <- decodeJSON "./validation.json"
  case validationData of
    Just ds -> runTestTTAndExit $ TestList [testJTD k v | (k,v) <- toList ds ]
    _ -> fail "invalid json"
