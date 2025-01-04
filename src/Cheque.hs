{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-

Write a function (dollars) that accepts a `String` and returns a `String`.
It will accept a numeric value as input, representing an amount of money, and convert to its transcribed English.

For example, the input "1.11" will result in a return value of "one dollar and eleven cents"

Invalid characters should be ignored, meaning that every input string has an output string.
The empty string produces "zero dollars and zero cents"

There is a `test` function below that lists more examples of input and output. There are also functions and
data structures that may assist you in deriving the result. It is not compulsory that they are used.

-}

module Cheque where

import qualified Applicative as A
import Core
import List (Chars, List (..))
import qualified List as L
import qualified Monad as M

-- $setup
-- >>> :set -XOverloadedStrings

-- The representation of the grouping of each exponent of one thousand. ["thousand", "million", ...]
illion ::
  List Chars
illion =
  let preillion ::
        List (Chars -> Chars)
      preillion =
        L.listh
          [ const "",
            const "un",
            const "do",
            const "tre",
            const "quattuor",
            const "quin",
            const "sex",
            const "septen",
            const "octo",
            \q -> if "n" `L.isPrefixOf` q then "novem" else "noven"
          ]
      postillion ::
        List Chars
      postillion =
        L.listh
          [ "vigintillion",
            "trigintillion",
            "quadragintillion",
            "quinquagintillion",
            "sexagintillion",
            "septuagintillion",
            "octogintillion",
            "nonagintillion",
            "centillion",
            "decicentillion",
            "viginticentillion",
            "trigintacentillion",
            "quadragintacentillion",
            "quinquagintacentillion",
            "sexagintacentillion",
            "septuagintacentillion",
            "octogintacentillion",
            "nonagintacentillion",
            "ducentillion",
            "deciducentillion",
            "vigintiducentillion",
            "trigintaducentillion",
            "quadragintaducentillion",
            "quinquagintaducentillion",
            "sexagintaducentillion",
            "septuagintaducentillion",
            "octogintaducentillion",
            "nonagintaducentillion",
            "trecentillion",
            "decitrecentillion",
            "vigintitrecentillion",
            "trigintatrecentillion",
            "quadragintatrecentillion",
            "quinquagintatrecentillion",
            "sexagintatrecentillion",
            "septuagintatrecentillion",
            "octogintatrecentillion",
            "nonagintatrecentillion",
            "quadringentillion",
            "deciquadringentillion",
            "vigintiquadringentillion",
            "trigintaquadringentillion",
            "quadragintaquadringentillion",
            "quinquagintaquadringentillion",
            "sexagintaquadringentillion",
            "septuagintaquadringentillion",
            "octogintaquadringentillion",
            "nonagintaquadringentillion",
            "quingentillion",
            "deciquingentillion",
            "vigintiquingentillion",
            "trigintaquingentillion",
            "quadragintaquingentillion",
            "quinquagintaquingentillion",
            "sexagintaquingentillion",
            "septuagintaquingentillion",
            "octogintaquingentillion",
            "nonagintaquingentillion",
            "sescentillion",
            "decisescentillion",
            "vigintisescentillion",
            "trigintasescentillion",
            "quadragintasescentillion",
            "quinquagintasescentillion",
            "sexagintasescentillion",
            "septuagintasescentillion",
            "octogintasescentillion",
            "nonagintasescentillion",
            "septingentillion",
            "deciseptingentillion",
            "vigintiseptingentillion",
            "trigintaseptingentillion",
            "quadragintaseptingentillion",
            "quinquagintaseptingentillion",
            "sexagintaseptingentillion",
            "septuagintaseptingentillion",
            "octogintaseptingentillion",
            "nonagintaseptingentillion",
            "octingentillion",
            "decioctingentillion",
            "vigintioctingentillion",
            "trigintaoctingentillion",
            "quadragintaoctingentillion",
            "quinquagintaoctingentillion",
            "sexagintaoctingentillion",
            "septuagintaoctingentillion",
            "octogintaoctingentillion",
            "nonagintaoctingentillion",
            "nongentillion",
            "decinongentillion",
            "vigintinongentillion",
            "trigintanongentillion",
            "quadragintanongentillion",
            "quinquagintanongentillion",
            "sexagintanongentillion",
            "septuagintanongentillion",
            "octogintanongentillion",
            "nonagintanongentillion"
          ]
   in L.listh
        [ "",
          "thousand",
          "million",
          "billion",
          "trillion",
          "quadrillion",
          "quintillion",
          "sextillion",
          "septillion",
          "octillion",
          "nonillion",
          "decillion",
          "undecillion",
          "duodecillion",
          "tredecillion",
          "quattuordecillion",
          "quindecillion",
          "sexdecillion",
          "septendecillion",
          "octodecillion",
          "novemdecillion"
        ]
        L.++ A.lift2 ((L.++) M.=<<) preillion postillion

-- A data type representing the digits zero to nine.
-- data Digit
--   = Zero
--   | One
--   | Two
--   | Three
--   | Four
--   | Five
--   | Six
--   | Seven
--   | Eight
--   | Nine
--   deriving stock (Eq, Ord, Show)

-- showDigit :: Digit -> Chars
-- showDigit = \case
--   Zero -> "zero"
--   One -> "one"
--   Two -> "two"
--   Three -> "three"
--   Four -> "four"
--   Five -> "five"
--   Six -> "six"
--   Seven -> "seven"
--   Eight -> "eight"
--   Nine -> "nine"

-- -- A data type representing one, two or three digits, which may be useful for grouping.
-- data Digit3
--   = D1 Digit
--   | D2 Digit Digit
--   | D3 Digit Digit Digit
--   deriving stock (Eq)

-- -- Possibly convert a character to a digit.
-- fromChar :: Char -> Optional Digit
-- fromChar = \case
--   '0' -> Full Zero
--   '1' -> Full One
--   '2' -> Full Two
--   '3' -> Full Three
--   '4' -> Full Four
--   '5' -> Full Five
--   '6' -> Full Six
--   '7' -> Full Seven
--   '8' -> Full Eight
--   '9' -> Full Nine
--   _ -> Empty

units :: List Chars
units =
  L.listh
    [ "one",
      "two",
      "three",
      "four",
      "five",
      "six",
      "seven",
      "eight",
      "nine"
    ]

specials :: List Chars
specials =
  L.listh
    [ "eleven",
      "twelve",
      "thirteen",
      "fourteen",
      "fifteen",
      "sixteen",
      "seventeen",
      "eighteen",
      "nineteen"
    ]

tens :: List Chars
tens =
  L.listh
    [ "ten",
      "twenty",
      "thirty",
      "forty",
      "fifty",
      "sixty",
      "seventy",
      "eighty",
      "ninety"
    ]

-- | Take a numeric value and produce its English output.
--
-- >>> dollars "0"
-- "zero dollars and zero cents"
--
-- >>> dollars "1"
-- "one dollar and zero cents"
--
-- >>> dollars "0.1"
-- "zero dollars and ten cents"
--
-- >>> dollars "1."
-- "one dollar and zero cents"
--
-- >>> dollars "0."
-- "zero dollars and zero cents"
--
-- >>> dollars "0.0"
-- "zero dollars and zero cents"
--
-- >>> dollars ".34"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "0.3456789"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "1.0"
-- "one dollar and zero cents"
--
-- >>> dollars "1.01"
-- "one dollar and one cent"
--
-- >>> dollars "a1a"
-- "one dollar and zero cents"
--
-- >>> dollars "a1a.a0.7b"
-- "one dollar and seven cents"
--
-- >>> dollars "100"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.0"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00000"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "1000456.13"
-- "one million four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "1001456.13"
-- "one million one thousand four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "16000000456.13"
-- "sixteen billion four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "100.45"
-- "one hundred dollars and forty-five cents"
--
-- >>> dollars "100.07"
-- "one hundred dollars and seven cents"
--
-- >>> dollars "9abc9def9ghi.jkl9mno"
-- "nine hundred and ninety-nine dollars and ninety cents"
--
-- >>> dollars "12345.67"
-- "twelve thousand three hundred and forty-five dollars and sixty-seven cents"
--
-- >>> dollars "456789123456789012345678901234567890123456789012345678901234567890.12"
-- "four hundred and fifty-six vigintillion seven hundred and eighty-nine novemdecillion one hundred and twenty-three octodecillion four hundred and fifty-six septendecillion seven hundred and eighty-nine sexdecillion twelve quindecillion three hundred and forty-five quattuordecillion six hundred and seventy-eight tredecillion nine hundred and one duodecillion two hundred and thirty-four undecillion five hundred and sixty-seven decillion eight hundred and ninety nonillion one hundred and twenty-three octillion four hundred and fifty-six septillion seven hundred and eighty-nine sextillion twelve quintillion three hundred and forty-five quadrillion six hundred and seventy-eight trillion nine hundred and one billion two hundred and thirty-four million five hundred and sixty-seven thousand eight hundred and ninety dollars and twelve cents"
dollars :: Chars -> Chars
dollars xs = fmt d "dollar" L.++ " and " L.++ fmt c "cent"
  where
    fmt amt curr
      | amt == "" = "zero " L.++ curr L.++ "s"
      | amt == "one" = amt L.++ " " L.++ curr
      | otherwise = amt L.++ " " L.++ curr L.++ "s"
    (whole, decimal) = parseDecimal xs
    d = words whole
    c = words decimal

words :: Chars -> Chars
words xs
  | (n + k) <= 3 = lessThanThou ys
  | otherwise = L.unwords (wl :. w :. (if L.isEmpty wr then Nil else wr :. Nil))
  where
    ys = lstrip xs
    ((n, l), (k, r)) = split ys
    w = elemAt (k `div` 3) illion
    wl = words l
    wr = words r

lessThanThou :: Chars -> Chars
lessThanThou xs =
  case ys of
    Nil -> Nil
    _ :. Nil -> elemAt (i - 1) units
    _ :. "0" -> elemAt (i - 1) tens
    '1' :. _ :. Nil -> elemAt (j - 1) specials
    _ :. _ :. Nil -> elemAt (i - 1) tens L.++ "-" L.++ elemAt (j - 1) units
    _ :. "00" -> elemAt (i - 1) units L.++ " hundred"
    _ -> L.unwords (elemAt (i - 1) units :. "hundred" :. "and" :. lessThanThou r :. Nil)
  where
    ys = lstrip xs
    (l, r) = uncons ys
    i = Core.digitToInt l
    j = Core.digitToInt (head r)

parseDecimal :: Chars -> (Chars, Chars)
parseDecimal xs = (whole, decimal)
  where
    (ys, zs) = L.span (/= '.') xs
    digits = L.filter isDigit
    whole = digits ys
    decimal = L.take 2 (digits zs L.++ "00")

type Group a = (Int, List a)

{-
Split into (prefix, suffix) such that the length
of the suffix is the longest multiple of three.

Examples:
1001 --> (1, 001)
999999 --> (999, 999)
1000001 --> (1, 000001)
-}
split :: List a -> (Group a, Group a)
split = L.foldRight f ((0, Nil), (0, Nil))
  where
    f x ((k, xs), acc@(n, ys)) =
      if k == 3
        then ((1, x :. Nil), (n + 3, xs L.++ ys))
        else ((k + 1, x :. xs), acc)

-- List helper functions.

lstrip :: Chars -> Chars
lstrip = L.dropWhile (== '0')

{-
The following are partial functions
that throw an error if the list doesn't
contain the required number of elements.
-}

elemAt :: Int -> List a -> a
elemAt n xs =
  case L.drop n xs of
    x :. _ -> x
    _ -> error "out of range"

head :: List a -> a
head = elemAt 0

uncons :: List a -> (a, List a)
uncons Nil = error "empty list"
uncons (x :. xs) = (x, xs)
