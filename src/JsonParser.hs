{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module JsonParser where

import qualified Applicative as A
import Core
import qualified Functor as F
import JsonValue
import List (Chars, FilePath, List (..))
import qualified List as L
import qualified Monad as M
import qualified MoreParser as MP
import Optional (Optional (..))
import Parser (ParseResult (..), Parser (..))
import qualified Parser as P

-- $setup
-- >>> :set -XOverloadedStrings

-- A special character is one of the following:

-- * \b  Backspace (ascii code 08)

-- * \f  Form feed (ascii code 0C)

-- * \n  New line

-- * \r  Carriage return

-- * \t  Tab

-- * \v  Vertical tab

-- * \'  Apostrophe or single quote (only valid in single quoted json strings)

-- * \"  Double quote (only valid in double quoted json strings)

-- * \\  Backslash character

-- https://www.json.org/json-en.html
data SpecialCharacter
  = BackSpace
  | FormFeed
  | NewLine
  | CarriageReturn
  | Tab
  | VerticalTab
  | SingleQuote
  | DoubleQuote
  | Backslash
  deriving stock (Eq, Ord, Show)

-- NOTE: This is not inverse to @toSpecialCharacter@.
fromSpecialCharacter ::
  SpecialCharacter ->
  Char
fromSpecialCharacter BackSpace =
  chr 0x08
fromSpecialCharacter FormFeed =
  chr 0x0C
fromSpecialCharacter NewLine =
  '\n'
fromSpecialCharacter CarriageReturn =
  '\r'
fromSpecialCharacter Tab =
  '\t'
fromSpecialCharacter VerticalTab =
  '\v'
fromSpecialCharacter SingleQuote =
  '\''
fromSpecialCharacter DoubleQuote =
  '"'
fromSpecialCharacter Backslash =
  '\\'

-- NOTE: This is not inverse to @fromSpecialCharacter@.
toSpecialCharacter ::
  Char ->
  Optional SpecialCharacter
toSpecialCharacter c =
  let table =
        ('b', BackSpace)
          :. ('f', FormFeed)
          :. ('n', NewLine)
          :. ('r', CarriageReturn)
          :. ('t', Tab)
          :. ('v', VerticalTab)
          :. ('\'', SingleQuote)
          :. ('"', DoubleQuote)
          :. ('\\', Backslash)
          :. Nil
   in snd F.<$> L.find ((==) c . fst) table

specialCharacterParser :: Parser Char
specialCharacterParser =
  P.character M.>>= \c ->
    if c == 'u'
      then MP.hex
      else case toSpecialCharacter c of
        Full x -> P.valueParser (fromSpecialCharacter x)
        _ -> P.constantParser (UnexpectedChar c)

-- | Parse a JSON string. Handle double-quotes, special characters, hexadecimal characters. See http://json.org for the full list of control characters in JSON.
--
-- /Tip:/ Use `hex`, `fromSpecialCharacter`, `between`, `is`, `charTok`, `toSpecialCharacter`.
--
-- >>> parse jsonString "\" abc\""
-- Result >< " abc"
--
-- >>> parse jsonString "\" abc\" "
-- Result >< " abc"
--
-- >>> parse jsonString "\"abc\"def"
-- Result >def< "abc"
--
-- >>> parse jsonString "\"\\babc\"def"
-- Result >def< "\babc"
--
-- >>> parse jsonString "\"\\u00abc\"def"
-- Result >def< "\171c"
--
-- >>> parse jsonString "\"\\u00ffabc\"def"
-- Result >def< "\255abc"
--
-- >>> parse jsonString "\"\\u00faabc\"def"
-- Result >def< "\250abc"
--
-- >>> isErrorResult (parse jsonString "abc")
-- True
--
-- >>> isErrorResult (parse jsonString "\"\\abc\"def")
-- True
jsonString :: Parser Chars
jsonString = MP.between (P.is '"') (P.is '"') (P.list ch)
  where
    ch =
      P.satisfy (/= '"') M.>>= \c ->
        if c == '\\'
          then specialCharacterParser
          else P.valueParser c

-- | Parse a JSON rational.
--
-- /Tip:/ Use @readFloats@.
--
-- /Optional:/ As an extra challenge, you may wish to support exponential notation
-- as defined on http://json.org/
-- This is not required.
--
-- >>> parse jsonNumber "234"
-- Result >< 234 % 1
--
-- >>> parse jsonNumber "234 "
-- Result >< 234 % 1
--
-- >>> parse jsonNumber "-234"
-- Result >< (-234) % 1
--
-- >>> parse jsonNumber "123.45"
-- Result >< 2469 % 20
--
-- >>> parse jsonNumber "-123"
-- Result >< (-123) % 1
--
-- >>> parse jsonNumber "-123.45"
-- Result >< (-2469) % 20
--
-- >>> isErrorResult (parse jsonNumber "-")
-- True
--
-- >>> isErrorResult (parse jsonNumber "abc")
-- True
jsonNumber :: Parser Rational
jsonNumber = P $ \s ->
  case L.readFloats s of
    Full (n, rest) -> Result rest n
    _ -> UnexpectedString s

-- | Parse a JSON true literal.
--
-- /Tip:/ Use `stringTok`.
--
-- >>> parse jsonTrue "true"
-- Result >< "true"
--
-- >>> isErrorResult (parse jsonTrue "TRUE")
-- True
jsonTrue :: Parser Chars
jsonTrue = MP.string "true"

-- | Parse a JSON false literal.
--
-- /Tip:/ Use `stringTok`.
--
-- >>> parse jsonFalse "false"
-- Result >< "false"
--
-- >>> isErrorResult (parse jsonFalse "FALSE")
-- True
jsonFalse :: Parser Chars
jsonFalse = MP.string "false"

-- | Parse a JSON null literal.
--
-- /Tip:/ Use `stringTok`.
--
-- >>> parse jsonNull "null"
-- Result >< "null"
--
-- >>> isErrorResult (parse jsonNull "NULL")
-- True
jsonNull :: Parser Chars
jsonNull = MP.string "null"

-- | Parse a JSON array.
--
-- /Tip:/ Use `betweenSepbyComma` and `jsonValue`.
--
-- >>> parse jsonArray "[]"
-- Result >< []
--
-- >>> parse jsonArray "[true]"
-- Result >< [JsonTrue]
--
-- >>> parse jsonArray "[true, \"abc\"]"
-- Result >< [JsonTrue,JsonString "abc"]
--
-- >>> parse jsonArray "[true, \"abc\", []]"
-- Result >< [JsonTrue,JsonString "abc",JsonArray []]
--
-- >>> parse jsonArray "[true, \"abc\", [false]]"
-- Result >< [JsonTrue,JsonString "abc",JsonArray [JsonFalse]]
jsonArray :: Parser (List JsonValue)
jsonArray = MP.betweenSepbyComma '[' ']' jsonValue

-- | Parse a JSON object.
--
-- /Tip:/ Use `jsonString`, `charTok`, `betweenSepbyComma` and `jsonValue`.
--
-- >>> parse jsonObject "{}"
-- Result >< []
--
-- >>> parse jsonObject "{ \"key1\" : true }"
-- Result >< [("key1",JsonTrue)]
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : false }"
-- Result >< [("key1",JsonTrue),("key2",JsonFalse)]
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : false } xyz"
-- Result >xyz< [("key1",JsonTrue),("key2",JsonFalse)]
jsonObject :: Parser Assoc
jsonObject = MP.betweenSepbyComma '{' '}' assoc
  where
    colon = MP.spaces A.*> MP.charTok ':'
    assoc = A.lift3 (\k _ v -> (k, v)) jsonString colon jsonValue

simpleValue :: Parser JsonValue
simpleValue =
  (JsonString F.<$> jsonString)
    P.||| (JsonRational F.<$> jsonNumber)
    P.||| (jsonTrue A.*> A.pure JsonTrue)
    P.||| (jsonFalse A.*> A.pure JsonFalse)
    P.||| (jsonNull A.*> A.pure JsonNull)

-- | Parse a JSON value.
--
-- /Tip:/ Use `spaces`, `jsonNull`, `jsonTrue`, `jsonFalse`, `jsonArray`, `jsonString`, `jsonObject` and `jsonNumber`.
--
-- >>> parse jsonValue "true"
-- Result >< JsonTrue
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : [7, false] }"
-- Result >< [("key1",JsonTrue),("key2",JsonArray [JsonRational (7 % 1),JsonFalse])]
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : [7, false] , \"key3\" : { \"key4\" : null } }"
-- Result >< [("key1",JsonTrue),("key2",JsonArray [JsonRational (7 % 1),JsonFalse]),("key3",JsonObject [("key4",JsonNull)])]
jsonValue :: Parser JsonValue
jsonValue =
  MP.spaces
    A.*> ( simpleValue
             P.||| (JsonArray F.<$> jsonArray)
             P.||| (JsonObject F.<$> jsonObject)
         )
    A.<* MP.spaces

-- | Read a file into a JSON value.
--
-- /Tip:/ Use @readFile@ and `jsonValue`.
readJsonValue :: FilePath -> IO (ParseResult JsonValue)
readJsonValue = (P.parse jsonValue F.<$>) . L.readFile
