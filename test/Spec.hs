{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit
import Data.JTD.Base

-- test1 = TestCase (assertEqual "hello test" 1 1)
test1 = TestCase $ do
          validationData <- decodeJSON "./validation.json"
          assertBool "validation" True

main :: IO ()
main = do
  runTestTTAndExit $ TestList [TestLabel "test1" test1]
