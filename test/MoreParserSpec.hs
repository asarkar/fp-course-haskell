{-# LANGUAGE OverloadedStrings #-}

module MoreParserSpec (spec) where

import qualified Data.Char as Ch
import List (List (..))
import qualified MoreParser as MP
import Parser (ParseResult (..))
import qualified Parser as P
import Test.Hspec

spec :: Spec
spec = do
  describe "spacesTest" $ do
    it "can parse zero spaces" $
      P.parse MP.spaces "abc" `shouldBe` Result "abc" ""
    it "can parse single space" $
      P.parse MP.spaces " abc" `shouldBe` Result "abc" " "
    it "can parse multiple spaces" $
      P.parse MP.spaces "   abc" `shouldBe` Result "abc" "   "

  describe "tokTest" $ do
    it "can parse input without spaces" $
      P.parse (MP.tok (P.is 'a')) "abc" `shouldBe` Result "bc" 'a'
    it "can parse single space" $
      P.parse (MP.tok (P.is 'a')) "a bc" `shouldBe` Result "bc" 'a'
    it "can parse multiple spaces" $
      P.parse (MP.tok (P.is 'a')) "a   bc" `shouldBe` Result "bc" 'a'

  describe "charTokTest" $ do
    it "fails when character does not match" $
      P.isErrorResult (P.parse (MP.charTok 'a') "dabc") `shouldBe` True
    it "parses matching character" $
      P.parse (MP.charTok 'a') "abc" `shouldBe` Result "bc" 'a'
    it "parses matching character, dropping space" $
      P.parse (MP.charTok 'a') "a bc" `shouldBe` Result "bc" 'a'
    it "parses matching character, dropping spaces" $
      P.parse (MP.charTok 'a') "a   bc" `shouldBe` Result "bc" 'a'

  describe "commaTokTest" $ do
    it "fails when character is not a comma" $
      P.isErrorResult (P.parse MP.commaTok "1,23") `shouldBe` True
    it "parses leading comma" $
      P.parse MP.commaTok ",123" `shouldBe` Result "123" ','
    it "parses leading comma, dropping space" $
      P.parse MP.commaTok ", 123" `shouldBe` Result "123" ','
    it "parses leading comma, dropping multiple spaces" $
      P.parse MP.commaTok ",   123" `shouldBe` Result "123" ','

  describe "quoteTest" $ do
    it "fails when character is not a single or double quote" $
      P.isErrorResult (P.parse MP.quote "abc") `shouldBe` True
    it "parses single quote" $
      P.parse MP.quote "'abc" `shouldBe` Result "abc" '\''
    it "parses double quote" $
      P.parse MP.quote "\"abc" `shouldBe` Result "abc" '"'

  describe "stringTest" $ do
    it "fails when string is not matched" $
      P.isErrorResult (P.parse (MP.string "abc") "bcdef") `shouldBe` True
    it "parses matching string, leaves remaining input" $
      P.parse (MP.string "abc") "abcdef" `shouldBe` Result "def" "abc"
    it "parses matching string" $
      P.parse (MP.string "abc") "abc" `shouldBe` Result "" "abc"

  describe "stringTokTest" $ do
    it "fails when string is not matched" $
      P.isErrorResult (P.parse (MP.stringTok "abc") "bc  ") `shouldBe` True
    it "parses matching string followed by zero spaces" $
      P.parse (MP.stringTok "abc") "abc" `shouldBe` Result "" "abc"
    it "parses matching string followed by many spaces" $
      P.parse (MP.stringTok "abc") "abc  " `shouldBe` Result "" "abc"

  describe "optionTest" $ do
    it "produces parsed value when parser succeeds" $
      P.parse (MP.option 'x' P.character) "abc" `shouldBe` Result "bc" 'a'
    it "produces given value when parser fails" $
      P.parse (MP.option 'x' P.character) "" `shouldBe` Result "" 'x'

  describe "digits1Test" $ do
    it "fails when no digits at start of input" $
      P.isErrorResult (P.parse MP.digits1 "abc123") `shouldBe` True
    it "succeeds on digits" $
      P.parse MP.digits1 "123" `shouldBe` Result "" "123"
    it "succeeds on digits, leaves remaining input" $
      P.parse MP.digits1 "123abc" `shouldBe` Result "abc" "123"

  describe "oneofTest" $ do
    it "fails when given character not in string" $
      P.isErrorResult (P.parse (MP.oneof "abc") "def") `shouldBe` True
    it "given character prefixes input" $
      P.parse (MP.oneof "abc") "bcdef" `shouldBe` Result "cdef" 'b'

  describe "noneofTest" $ do
    it "fails when one of given characters prefixes input" $
      P.isErrorResult (P.parse (MP.noneof "abcd") "abc") `shouldBe` True
    it "succeeds when none of the given characters in input" $
      P.parse (MP.noneof "xyz") "abc" `shouldBe` Result "bc" 'a'
    it "succeeds when none of the given characters prefixes input" $
      P.parse (MP.noneof "bcd") "abc" `shouldBe` Result "bc" 'a'

  describe "betweenTest" $ do
    it "fails when opening parse fails" $
      P.isErrorResult (P.parse (MP.between (P.is '[') (P.is ']') P.character) "abc]") `shouldBe` True
    it "fails when surrounded parser fails" $
      P.isErrorResult (P.parse (MP.between (P.is '[') (P.is ']') P.character) "[abc]") `shouldBe` True
    it "fails when closing parse fails" $
      P.isErrorResult (P.parse (MP.between (P.is '[') (P.is ']') P.character) "[abc") `shouldBe` True
    it "succeeds: character surrounded by []'" $
      P.parse (MP.between (P.is '[') (P.is ']') P.character) "[a]" `shouldBe` Result "" 'a'
    it "succeeds: digits surrounded by []" $
      P.parse (MP.between (P.is '[') (P.is ']') MP.digits1) "[123]" `shouldBe` Result "" "123"

  describe "betweenCharTokTest" $ do
    it "fails when opening character not present" $
      P.isErrorResult (P.parse (MP.betweenCharTok '[' ']' P.character) "abc]") `shouldBe` True
    it "fails when closing character not present" $
      P.isErrorResult (P.parse (MP.betweenCharTok '[' ']' P.character) "[abc") `shouldBe` True
    it "fails when surrounded parser fails" $
      P.isErrorResult (P.parse (MP.betweenCharTok '[' ']' P.character) "[abc]") `shouldBe` True
    it "succeeds: character" $
      P.parse (MP.betweenCharTok '[' ']' P.character) "[a]" `shouldBe` Result "" 'a'
    it "succeeds: digits1" $
      P.parse (MP.betweenCharTok '[' ']' MP.digits1) "[123]" `shouldBe` Result "" "123"

  describe "hexTest" $ do
    it "fails on invalid hex string --- too short" $
      P.isErrorResult (P.parse MP.hex "001") `shouldBe` True
    it "fails on invalid hex string --- invalid char (x)" $
      P.isErrorResult (P.parse MP.hex "0axf") `shouldBe` True
    it "succeeds on valid hex value" $
      P.parse MP.hex "0010" `shouldBe` Result "" '\DLE'

  describe "hexuTest" $ do
    it "fails when no u at start" $
      P.isErrorResult (P.parse MP.hexu "0010") `shouldBe` True
    it "fails when not 4 hex digits after u" $
      P.isErrorResult (P.parse MP.hexu "u010") `shouldBe` True
    it "fails on invalid hex digit" $
      P.isErrorResult (P.parse MP.hexu "u0axf") `shouldBe` True
    it "succeeds on valid input --- u0010" $
      P.parse MP.hexu "u0010" `shouldBe` Result "" '\DLE'
    it "succeeds on valid input --- u0a1f" $
      P.parse MP.hexu "u0a1f" `shouldBe` Result "" '\2591'

  describe "sepby1Test" $ do
    it "fails when first parser fails" $
      P.isErrorResult (P.parse (MP.sepby1 P.character (P.is ',')) "") `shouldBe` True
    it "parses single character not followed by seperator" $
      P.parse (MP.sepby1 P.character (P.is ',')) "a" `shouldBe` Result "" "a"
    it "parses multiple matches with separators" $
      P.parse (MP.sepby1 P.character (P.is ',')) "a,b,c" `shouldBe` Result "" "abc"
    it "succeeds until two separators" $
      P.parse (MP.sepby1 P.character (P.is ',')) "a,b,c,,def" `shouldBe` Result "def" "abc,"

    -- Additional tests added by me
    it "parses single character followed by seperator" $
      P.parse (MP.sepby1 P.character (P.is ',')) "a," `shouldBe` Result "," "a"
    it "parses only seperator" $
      P.parse (MP.sepby1 P.character (P.is ',')) "," `shouldBe` Result "" ","

  describe "sepbyTest" $ do
    it "succeeds on empty string" $
      P.parse (MP.sepby P.character (P.is ',')) "" `shouldBe` Result "" ""
    it "succeeds on single match without seperator" $
      P.parse (MP.sepby P.character (P.is ',')) "a" `shouldBe` Result "" "a"
    it "succeeds on multiple matches with seperators" $
      P.parse (MP.sepby P.character (P.is ',')) "a,b,c" `shouldBe` Result "" "abc"
    it "succeeds until two separators" $
      P.parse (MP.sepby P.character (P.is ',')) "a,b,c,,def" `shouldBe` Result "def" "abc,"

    -- Additional tests added by me
    it "parses single character followed by seperator" $
      P.parse (MP.sepby P.character (P.is ',')) "a," `shouldBe` Result "," "a"
    it "parses only seperator" $
      P.parse (MP.sepby P.character (P.is ',')) "," `shouldBe` Result "" ","

  describe "eofTest" $ do
    it "fails when still input left" $
      P.isErrorResult (P.parse MP.eof "abc") `shouldBe` True
    it "succeeds when no input left" $
      P.parse MP.eof "" `shouldBe` Result "" ()

  describe "satisfyAllTest" $ do
    it "fails when a predicate fails" $
      P.isErrorResult (P.parse (MP.satisfyAll (Ch.isUpper :. (/= 'X') :. Nil)) "XBc") `shouldBe` True
    it "fails when no predicates satisfied (empty input)" $
      P.isErrorResult (P.parse (MP.satisfyAll (Ch.isUpper :. (/= 'X') :. Nil)) "") `shouldBe` True
    it "fails when no predicates satisfied" $
      P.isErrorResult (P.parse (MP.satisfyAll (Ch.isUpper :. (/= 'X') :. Nil)) "abc") `shouldBe` True
    it "succeeds when all predicates satisfied: ABC" $
      P.parse (MP.satisfyAll (Ch.isUpper :. (/= 'X') :. Nil)) "ABC" `shouldBe` Result "BC" 'A'
    it "succeeds when all predicates satisfied: ABc" $
      P.parse (MP.satisfyAll (Ch.isUpper :. (/= 'X') :. Nil)) "ABc" `shouldBe` Result "Bc" 'A'

  describe "satisfyAnyTest" $ do
    it "fails when no predicates satisfied" $
      P.isErrorResult (P.parse (MP.satisfyAny (Ch.isLower :. (/= 'X') :. Nil)) "XBc") `shouldBe` True
    it "fails when no predicates satisfied (empty input)" $
      P.isErrorResult (P.parse (MP.satisfyAny (Ch.isLower :. (/= 'X') :. Nil)) "") `shouldBe` True
    it "succeeds when all predicates satisfied" $
      P.parse (MP.satisfyAny (Ch.isUpper :. (/= 'X') :. Nil)) "ABc" `shouldBe` Result "Bc" 'A'
    it "succeeds when one of two predicates satisfied" $
      P.parse (MP.satisfyAny (Ch.isLower :. (/= 'X') :. Nil)) "ABc" `shouldBe` Result "Bc" 'A'

  describe "betweenSepbyCommaTest" $ do
    it "fails when opening char missing" $
      P.isErrorResult (P.parse (MP.betweenSepbyComma '[' ']' P.lower) "a]") `shouldBe` True
    it "fails when closing char missing" $
      P.isErrorResult (P.parse (MP.betweenSepbyComma '[' ']' P.lower) "[a") `shouldBe` True
    it "fails when input between seperators doesn't match (multiple matches)" $
      P.isErrorResult (P.parse (MP.betweenSepbyComma '[' ']' P.lower) "[abc]") `shouldBe` True
    it "fails when input between seperators doesn't match" $
      P.isErrorResult (P.parse (MP.betweenSepbyComma '[' ']' P.lower) "[A]") `shouldBe` True
    it "succeeds --- one match" $
      P.parse (MP.betweenSepbyComma '[' ']' P.lower) "[a]" `shouldBe` Result "" "a"
    it "succeeds --- nothing between surrounds" $
      P.parse (MP.betweenSepbyComma '[' ']' P.lower) "[]" `shouldBe` Result "" ""
    it "succeeds --- 3 matches" $
      P.parse (MP.betweenSepbyComma '[' ']' P.lower) "[a,b,c]" `shouldBe` Result "" "abc"
    it "succeeds --- 3 padded matches" $
      P.parse (MP.betweenSepbyComma '[' ']' P.lower) "[a,  b, c]" `shouldBe` Result "" "abc"
    it "succeeds --- digits1" $
      P.parse (MP.betweenSepbyComma '[' ']' MP.digits1) "[123,456]" `shouldBe` Result "" ("123" :. "456" :. Nil)
