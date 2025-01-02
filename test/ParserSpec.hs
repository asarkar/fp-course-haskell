{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import qualified Applicative as A
import qualified Data.Char as Ch
import qualified Functor as F
import List (List (..))
import qualified Monad as M
import Optional (Optional (..))
import Parser (ParseResult (..), Parser (..), (|||))
import qualified Parser as P
import Person
import Test.Hspec

spec :: Spec
spec = do
  describe "constantParserTest" $ do
    it "can return error result" $
      P.parse (P.constantParser (UnexpectedEof :: ParseResult Int)) "abc" `shouldBe` (UnexpectedEof :: ParseResult Int)
    it "can return ParseResult" $
      P.parse (P.constantParser (Result "xyz" 4)) "abc" `shouldBe` Result "xyz" 4

  describe "characterTest" $ do
    it "parses single character from non-empty string" $
      P.parse P.character "abc" `shouldBe` Result "bc" 'a'
    it "parsing empty string is an error" $
      P.isErrorResult (P.parse P.character "") `shouldBe` True

  describe "functorTest" $ do
    it "toUpper <$>" $
      P.parse (Ch.toUpper F.<$> P.character) "amz" `shouldBe` Result "mz" 'A'

  describe "valueParserTest" $ do
    it "succeeds with given value" $
      P.parse (P.valueParser 3) "abc" `shouldBe` Result "abc" 3

  describe "alternativeParserTest" $ do
    it "first fails, second succeeds with no input" $
      P.parse (P.character ||| P.valueParser 'v') "" `shouldBe` Result "" 'v'
    it "first always fails, second succeeds with no input" $
      P.parse (P.constantParser UnexpectedEof ||| P.valueParser 'v') "" `shouldBe` Result "" 'v'
    it "first always fails, second succeeds with input" $
      P.parse (P.constantParser UnexpectedEof ||| P.valueParser 'v') "abc" `shouldBe` Result "abc" 'v'
    it "takes first parse result when it succeeds" $
      P.parse (P.character ||| P.valueParser 'v') "abc" `shouldBe` Result "bc" 'a'

  describe "parserMonadInstanceTest" $ do
    it "first parse fails" $
      P.isErrorResult (P.parse ((\c -> if c == 'x' then P.character else P.valueParser 'v') M.=<< P.character) "") `shouldBe` True
    it "second parse fails" $
      P.isErrorResult (P.parse ((\c -> if c == 'x' then P.character else P.valueParser 'v') M.=<< P.character) "x") `shouldBe` True
    it "bind to valueParser" $
      P.parse ((\c -> if c == 'x' then P.character else P.valueParser 'v') M.=<< P.character) "abc" `shouldBe` Result "bc" 'v'
    it "bind to valueParser with no more input" $
      P.parse ((\c -> if c == 'x' then P.character else P.valueParser 'v') M.=<< P.character) "a" `shouldBe` Result "" 'v'
    it "bind to character parser with remaining input" $
      P.parse ((\c -> if c == 'x' then P.character else P.valueParser 'v') M.=<< P.character) "xabc" `shouldBe` Result "bc" 'a'

  describe "parserApplicativeInstanceTest" $ do
    it "pure" $
      P.parse (A.pure 'a' :: Parser Char) "xyz" `shouldBe` Result "xyz" 'a'
    it "pure an Optional value" $
      P.parse (A.pure (Full 5) :: Parser (Optional Int)) "xyz" `shouldBe` Result "xyz" (Full 5)
    it "pure toUpper <*>" $
      P.parse (A.pure Ch.toUpper A.<*> P.valueParser 'a') "xyz" `shouldBe` Result "xyz" 'A'
    it "pure show <*>" $
      P.parse (A.pure show A.<*> P.valueParser 599) "xyz" `shouldBe` Result "xyz" "599"
    it "append character <*>" $
      P.parse (((\a b -> a :. b :. Nil) F.<$> P.character) A.<*> P.character) "abxyz" `shouldBe` Result "xyz" "ab"

  describe "satisfyTest" $ do
    it "when character satisfies predicate" $
      P.parse (P.satisfy Ch.isUpper) "Abc" `shouldBe` Result "bc" 'A'
    it "is error when predicate not satisfied" $
      P.isErrorResult (P.parse (P.satisfy Ch.isUpper) "abc") `shouldBe` True

  describe "digitTest" $ do
    it "is error when input empty" $
      P.isErrorResult (P.parse P.digit "") `shouldBe` True
    it "is error when character not digit" $
      P.isErrorResult (P.parse P.digit "ABC") `shouldBe` True
    it "succeeds when character is a digit  " $
      P.parse P.digit "1BC" `shouldBe` Result "BC" '1'

  describe "spaceTest" $ do
    it "fails when input empty" $
      P.isErrorResult (P.parse P.space "") `shouldBe` True
    it "fails when character not space" $
      P.isErrorResult (P.parse P.space "ABC") `shouldBe` True
    it "succeeds when first character is a space" $
      P.parse P.space " abc" `shouldBe` Result "abc" ' '

  describe "listTest" $ do
    it "succeeds on empty input" $
      P.parse (P.list P.character) "" `shouldBe` Result "" ""
    it "parses for as long as characters match" $
      P.parse (P.list P.digit) "123abc" `shouldBe` Result "abc" "123"
    it "parses empty value when no matching characters" $
      P.parse (P.list P.digit) "abc" `shouldBe` Result "abc" ""
    it "parses entire input if matches" $
      P.parse (P.list P.character) "abc" `shouldBe` Result "" "abc"
    it "parses for as long as characters match" $
      P.parse (P.list (P.character A.*> P.valueParser 'v')) "abc" `shouldBe` Result "" "vvv"
    it "succeeds on empty input with value parser" $
      P.parse (P.list (P.character A.*> P.valueParser 'v')) "" `shouldBe` Result "" ""

  describe "list1Test" $ do
    it "succeeds when at least one character matches" $
      P.parse (P.list1 P.character) "abc" `shouldBe` Result "" "abc"
    it "succeeds when at least one character matches" $
      P.parse (P.list1 (P.character A.*> P.valueParser 'v')) "abc" `shouldBe` Result "" "vvv"
    it "no matching chars fails" $
      P.isErrorResult (P.parse (P.list1 (P.character A.*> P.valueParser 'v')) "") `shouldBe` True

  describe "spaces1Test" $ do
    it "fails on empty string" $
      P.isErrorResult (P.parse P.spaces1 "") `shouldBe` True
    it "consumes single space" $
      P.parse P.spaces1 " " `shouldBe` Result "" " "
    it "consumes multiple spaces" $
      P.parse P.spaces1 "    abc" `shouldBe` Result "abc" "    "

  describe "lowerTest" $ do
    it "fails on empty string" $
      P.isErrorResult (P.parse P.lower "") `shouldBe` True
    it "fails if character is not lowercase" $
      P.isErrorResult (P.parse P.lower "Abc") `shouldBe` True
    it "produces lowercase character" $
      P.parse P.lower "aBC" `shouldBe` Result "BC" 'a'

  describe "upperTest" $ do
    it "fails on empty string" $
      P.isErrorResult (P.parse P.upper "") `shouldBe` True
    it "fails if character is not uppercase" $
      P.isErrorResult (P.parse P.upper "aBC") `shouldBe` True
    it "produces uppercase character" $
      P.parse P.upper "Abc" `shouldBe` Result "bc" 'A'

  describe "alphaTest" $ do
    it "fails on empty string" $
      P.isErrorResult (P.parse P.alpha "") `shouldBe` True
    it "fails if character is not alpha" $
      P.isErrorResult (P.parse P.alpha "5BC") `shouldBe` True
    it "produces alpha character" $
      P.parse P.alpha "A45" `shouldBe` Result "45" 'A'

  describe "sequenceParserTest" $ do
    it "fails on first failing parser" $
      P.isErrorResult (P.parse (P.sequenceParser (P.character :. P.is 'x' :. P.upper :. Nil)) "abCdef") `shouldBe` True
    it "sequences list of successful parsers" $
      P.parse (P.sequenceParser (P.character :. P.is 'x' :. P.upper :. Nil)) "axCdef" `shouldBe` Result "def" "axC"

  describe "thisManyTest" $ do
    it "fails when not enough matches" $
      P.isErrorResult (P.parse (P.thisMany 4 P.upper) "ABcDef") `shouldBe` True
    it "produces n values when matched" $
      P.parse (P.thisMany 4 P.upper) "ABCDef" `shouldBe` Result "ef" "ABCD"

  describe "ageParserTest (done for you)" $ do
    it "fails on invalid age (all letters)" $
      P.isErrorResult (P.parse P.ageParser "abc") `shouldBe` True
    it "fails on invalid age (leading '-')" $
      P.isErrorResult (P.parse P.ageParser "-120") `shouldBe` True
    it "parses valid age" $
      P.parse P.ageParser "120" `shouldBe` Result "" 120

  describe "firstNameParserTest" $ do
    it "fails on first name that doesn't start with a capital" $
      P.isErrorResult (P.parse P.firstNameParser "abc") `shouldBe` True
    it "parses valid first name" $
      P.parse P.firstNameParser "Abc" `shouldBe` Result "" "Abc"

  describe "surnameParserTest" $ do
    it "fails on short surname" $
      P.isErrorResult (P.parse P.surnameParser "Abc") `shouldBe` True
    it "fails on short surname starting with a lower case letter" $
      P.isErrorResult (P.parse P.surnameParser "abc") `shouldBe` True
    it "parses shortest valid surname" $
      P.parse P.surnameParser "Abcdef" `shouldBe` Result "" "Abcdef"
    it "parses long surname" $
      P.parse P.surnameParser "Abcdefghijklmnopqrstuvwxyz" `shouldBe` Result "" "Abcdefghijklmnopqrstuvwxyz"

  describe "smokerParserTest" $ do
    it "fails on non y/n value" $
      P.isErrorResult (P.parse P.smokerParser "abc") `shouldBe` True
    it "parses y, leaving remaining input" $
      P.parse P.smokerParser "yabc" `shouldBe` Result "abc" True
    it "parses n, leaving remaining input" $
      P.parse P.smokerParser "nabc" `shouldBe` Result "abc" False

  describe "phoneBodyParserTest" $ do
    it "produces empty list when no characters match" $
      P.parse P.phoneBodyParser "a123-456" `shouldBe` Result "a123-456" ""
    it "parses valid phone body value" $
      P.parse P.phoneBodyParser "123-456" `shouldBe` Result "" "123-456"
    it "parses up to first letter" $
      P.parse P.phoneBodyParser "123-a456" `shouldBe` Result "a456" "123-"

  describe "phoneParserTest" $ do
    it "fails without trailing '#'" $
      P.isErrorResult (P.parse P.phoneParser "123-456") `shouldBe` True
    it "fails when input starts with a letter" $
      P.isErrorResult (P.parse P.phoneParser "a123-456") `shouldBe` True
    it "produces valid phone number" $
      P.parse P.phoneParser "123-456#" `shouldBe` Result "" "123-456"
    it "produces a valid phone number with remaining input" $
      P.parse P.phoneParser "123-456#abc" `shouldBe` Result "abc" "123-456"

  describe "personParserTest" $ do
    it "fails on empty string" $
      P.isErrorResult (P.parse P.personParser "") `shouldBe` True
    it "fails on invalid age" $
      P.isErrorResult (P.parse P.personParser "12x Fred Clarkson y 123-456.789#") `shouldBe` True
    it "fails on first name that doesn't start with capital" $
      P.isErrorResult (P.parse P.personParser "123 fred Clarkson y 123-456.789#") `shouldBe` True
    it "fails on surname that is too short" $
      P.isErrorResult (P.parse P.personParser "123 Fred Cla y 123-456.789#") `shouldBe` True
    it "fails on surname that doesn't start with a capital letter" $
      P.isErrorResult (P.parse P.personParser "123 Fred clarkson y 123-456.789#") `shouldBe` True
    it "fails on invalid smoker value 'x'" $
      P.isErrorResult (P.parse P.personParser "123 Fred Clarkson x 123-456.789#") `shouldBe` True
    it "fails on phone number containing an 'x'" $
      P.isErrorResult (P.parse P.personParser "123 Fred Clarkson y 1x3-456.789#") `shouldBe` True
    it "fails on phone number starting with '-'" $
      P.isErrorResult (P.parse P.personParser "123 Fred Clarkson y -123-456.789#") `shouldBe` True
    it "fails on phone number without a trailing '#'" $
      P.isErrorResult (P.parse P.personParser "123 Fred Clarkson y 123-456.789") `shouldBe` True
    it "produces person for valid input" $
      P.parse P.personParser "123 Fred Clarkson y 123-456.789#"
        `shouldBe` Result "" (Person 123 "Fred" "Clarkson" True "123-456.789")
    it "produces person for valid input and keeps remaining input" $
      P.parse P.personParser "123 Fred Clarkson y 123-456.789# rest"
        `shouldBe` Result " rest" (Person 123 "Fred" "Clarkson" True "123-456.789")
    it "produces person for valid input containing extra whitespace" $
      P.parse P.personParser "123  Fred   Clarkson    y     123-456.789#"
        `shouldBe` Result "" (Person 123 "Fred" "Clarkson" True "123-456.789")
