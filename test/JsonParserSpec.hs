{-# LANGUAGE OverloadedStrings #-}

module JsonParserSpec (spec) where

import Data.Ratio ((%))
import qualified JsonParser as JP
import JsonValue
import List (List (..))
import Parser (ParseResult (..))
import qualified Parser as P
import Test.Hspec

spec :: Spec
spec = do
  describe "specialCharacterParser" $ do
    it "fail when the input is empty" $
      P.isErrorResult (P.parse JP.specialCharacterParser "") `shouldBe` True
    it "fail on regular character" $
      P.isErrorResult (P.parse JP.specialCharacterParser "abc") `shouldBe` True
    it "fail on invalid special character" $
      P.isErrorResult (P.parse JP.specialCharacterParser "abc") `shouldBe` True
    it "fail on invalid hex string --- too short" $
      P.isErrorResult (P.parse JP.specialCharacterParser "u001") `shouldBe` True
    it "fail on invalid hex string --- invalid char (x)" $
      P.isErrorResult (P.parse JP.specialCharacterParser "u0axf") `shouldBe` True
    it "succeed on valid hex value" $
      P.parse JP.specialCharacterParser "u0010bc" `shouldBe` Result "bc" '\DLE'
    it "succeed on  back slash (\\)" $
      P.parse JP.specialCharacterParser "babc\"def" `shouldBe` Result "abc\"def" '\b'

  describe "jsonString" $ do
    it "parse whole ASCII input" $
      P.parse JP.jsonString "\" abc\"" `shouldBe` Result "" " abc"
    it "parse only the first string of input" $
      P.parse JP.jsonString "\"abc\"def" `shouldBe` Result "def" "abc"
    it "parse back slash (\\)" $
      P.parse JP.jsonString "\"\\babc\"def" `shouldBe` Result "def" "\babc"
    it "parse unicode (\\u00abc)" $
      P.parse JP.jsonString "\"\\u00abc\"def" `shouldBe` Result "def" "«c"
    it "parse unicode (\\u00ff)" $
      P.parse JP.jsonString "\"\\u00ffabc\"def" `shouldBe` Result "def" "ÿabc"
    it "parse unicode (\\u00fa)" $
      P.parse JP.jsonString "\"\\u00faabc\"def" `shouldBe` Result "def" "úabc"
    it "parsing string without quotes is an error" $
      P.isErrorResult (P.parse JP.jsonString "abc") `shouldBe` True
    it "parsing string containing \\a is an error - \\a isn't a special character" $
      P.isErrorResult (P.parse JP.jsonString "\"\\abc\"def") `shouldBe` True

  describe "jsonNumber" $ do
    it "positive whole" $ P.parse JP.jsonNumber "234" `shouldBe` Result "" (234 % 1)
    it "negative whole" $ P.parse JP.jsonNumber "-234" `shouldBe` Result "" ((-234) % 1)
    it "positive decimal" $ P.parse JP.jsonNumber "123.45" `shouldBe` Result "" (2469 % 20)
    it "negative whole (2)" $ P.parse JP.jsonNumber "-123" `shouldBe` Result "" ((-123) % 1)
    it "negative decimal" $ P.parse JP.jsonNumber "-123.45" `shouldBe` Result "" ((-2469) % 20)
    it "negative sign on its own is error" $ P.isErrorResult (P.parse JP.jsonNumber "-") `shouldBe` True
    it "alphabetic characters is error" $ P.isErrorResult (P.parse JP.jsonNumber "abc") `shouldBe` True

  describe "jsonTrue" $ do
    it "parses true" $ P.parse JP.jsonTrue "true" `shouldBe` Result "" "true"
    it "TRUE (caps) is an error" $ P.isErrorResult (P.parse JP.jsonTrue "TRUE") `shouldBe` True

  describe "jsonFalse" $ do
    it "parses false" $ P.parse JP.jsonFalse "false" `shouldBe` Result "" "false"
    it "FALSE (caps) is an error" $ P.isErrorResult (P.parse JP.jsonFalse "FALSE") `shouldBe` True

  describe "jsonNull" $ do
    it "parses null" $ P.parse JP.jsonNull "null" `shouldBe` Result "" "null"
    it "NULL (caps) is an error" $ P.isErrorResult (P.parse JP.jsonNull "NULL") `shouldBe` True

  describe "jsonArray" $ do
    it "[]" $
      P.parse JP.jsonArray "[]" `shouldBe` Result "" Nil
    it "[true]" $
      P.parse JP.jsonArray "[true]" `shouldBe` Result "" (JsonTrue :. Nil)
    it "[true, \"abc\"]" $
      P.parse JP.jsonArray "[true, \"abc\"]" `shouldBe` Result "" (JsonTrue :. JsonString "abc" :. Nil)
    it "[true, \"abc\", []]" $
      P.parse JP.jsonArray "[true, \"abc\", []]" `shouldBe` Result "" (JsonTrue :. JsonString "abc" :. JsonArray Nil :. Nil)
    it "[true, \"abc\", [false]]" $
      let result = Result "" (JsonTrue :. JsonString "abc" :. JsonArray (JsonFalse :. Nil) :. Nil)
       in P.parse JP.jsonArray "[true, \"abc\", [false]]" `shouldBe` result

  describe "jsonObject" $ do
    it "empty" $
      P.parse JP.jsonObject "{}" `shouldBe` Result "" Nil
    it "one key" $
      P.parse JP.jsonObject "{ \"key1\" : true }" `shouldBe` Result "" (("key1", JsonTrue) :. Nil)
    it "two keys" $
      P.parse JP.jsonObject "{ \"key1\" : true , \"key2\" : false }" `shouldBe` Result "" (("key1", JsonTrue) :. ("key2", JsonFalse) :. Nil)
    it "two keys and left over input" $
      let result = Result "xyz" (("key1", JsonTrue) :. ("key2", JsonFalse) :. Nil)
       in P.parse JP.jsonObject "{ \"key1\" : true , \"key2\" : false } xyz" `shouldBe` result

  describe "jsonValue" $ do
    it "true" $
      P.parse JP.jsonValue "true" `shouldBe` Result "" JsonTrue
    it "object" $
      let result =
            Result
              ""
              ( ("key1", JsonTrue)
                  :. ("key2", JsonArray (JsonRational (7 % 1) :. JsonFalse :. Nil))
                  :. Nil
              )
       in P.parse JP.jsonObject "{ \"key1\" : true , \"key2\" : [7, false] }" `shouldBe` result
    it "nested object" $
      let result =
            Result
              ""
              ( ("key1", JsonTrue)
                  :. ("key2", JsonArray (JsonRational (7 % 1) :. JsonFalse :. Nil))
                  :. ("key3", JsonObject (("key4", JsonNull) :. Nil))
                  :. Nil
              )
       in P.parse JP.jsonObject "{ \"key1\" : true , \"key2\" : [7, false] , \"key3\" : { \"key4\" : null } }" `shouldBe` result
