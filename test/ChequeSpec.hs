{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS -Wno-x-partial #-}
{-# OPTIONS -Wno-incomplete-uni-patterns #-}

module ChequeSpec (spec) where

import qualified Cheque as Ch
import qualified Control.Exception as E
import qualified Control.Monad as CM
import qualified Data.List as L'
import List (List (..))
import qualified List as L
import Property ()
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

zipWithIndices :: List a -> [(a, Int)]
zipWithIndices xs = zip (L.hlist xs) [0 ..]

spec :: Spec
spec = do
  describe "lstrip" $ do
    it "no-op on an empty string" $
      Ch.lstrip "" `shouldBe` ""
    it "strips a string that only contains zeros" $
      Ch.lstrip "000" `shouldBe` ""
    it "doesn't strip intermediate zeros" $
      Ch.lstrip "a00b" `shouldBe` "a00b"
    it "doesn't strip zeros at the right" $
      Ch.lstrip "ab0" `shouldBe` "ab0"

  describe "elemAt" $ do
    prop "can't get an element from an empty list" $
      -- We can generate multiple arguments:
      -- \(NonNegative (n :: Int)) (xs :: List Integer) -> do
      \(NonNegative (n :: Int)) ->
        E.evaluate (Ch.elemAt n (Nil :: List Integer)) `shouldThrow` anyErrorCall
    prop "gets an element from a non-empty list" $
      \(NonEmpty (xs :: [Int])) ->
        -- n is dependent on (length xs), so needs
        -- to be generated separately.
        forAll (chooseInt (0, L'.length xs - 1)) $ \n ->
          Ch.elemAt n (L.listh xs) `shouldBe` xs L'.!! n

  describe "head" $ do
    prop "can't get the head an empty list" $
      E.evaluate (Ch.head (Nil :: List Integer)) `shouldThrow` anyErrorCall

    prop "gets the head of a non-empty list" $
      \(NonEmpty (xs :: [Int])) ->
        Ch.head (L.listh xs) `shouldBe` L'.head xs

  describe "uncons" $ do
    prop "can't decompose an empty list" $
      E.evaluate (Ch.uncons (Nil :: List Integer)) `shouldThrow` anyErrorCall

    prop "decomposes a non-empty list" $
      \(NonEmpty (xs :: [Int])) -> do
        let (y, ys) = Ch.uncons (L.listh xs)
            Just (z, zs) = L'.uncons xs

        (y, L.hlist ys) `shouldBe` (z, zs)

  describe "split" $ do
    let xs =
          [ ("", ((0, ""), (0, ""))),
            ("0", ((1, "0"), (0, ""))),
            ("101", ((3, "101"), (0, ""))),
            ("1001", ((1, "1"), (3, "001"))),
            ("999999", ((3, "999"), (3, "999"))),
            ("1000001", ((1, "1"), (6, "000001")))
          ]

    CM.forM_ xs $ \(x, expected) -> do
      it ("splits \"" ++ x ++ "\"") $
        Ch.split (L.listh x) `shouldBe` expected

  describe "parseDecimal" $ do
    let xs =
          [ ("0", ("0", "00")),
            ("0.1", ("0", "10")),
            (".1", ("", "10")),
            ("0.", ("0", "00")),
            (".34", ("", "34")),
            ("0.3456789", ("0", "34")),
            ("1.1", ("1", "10")),
            ("1.01", ("1", "01")),
            ("1.00", ("1", "00")),
            ("1.001", ("1", "00")),
            ("a1a.a0.7b", ("1", "07")),
            ("100", ("100", "00")),
            ("100.0", ("100", "00")),
            ("000001.02", ("000001", "02"))
          ]

    CM.forM_ xs $ \(x, expected) -> do
      it ("parses \"" ++ x ++ "\"") $
        Ch.parseDecimal (L.listh x) `shouldBe` expected

  describe "lessThanThou" $ do
    CM.forM_ (zipWithIndices Ch.units) $ \(x, i) -> do
      it ("monetize \"" ++ show (i + 1) ++ "\"") $
        Ch.lessThanThou (L.show' (i + 1)) `shouldBe` x

    CM.forM_ (zipWithIndices Ch.specials) $ \(x, i) -> do
      it ("monetize \"" ++ show (i + 11) ++ "\"") $
        Ch.lessThanThou (L.show' (i + 11)) `shouldBe` x

    CM.forM_ (zipWithIndices Ch.tens) $ \(x, i) -> do
      it ("monetize \"" ++ show ((i + 1) * 10) ++ "\"") $
        Ch.lessThanThou (L.show' ((i + 1) * 10)) `shouldBe` x

    let xs =
          [ ("0", []),
            ("29", "twenty-nine"),
            ("73", "seventy-three"),
            ("110", "one hundred and ten"),
            ("111", "one hundred and eleven"),
            ("333", "three hundred and thirty-three"),
            ("705", "seven hundred and five")
          ]

    CM.forM_ xs $ \(x, expected) -> do
      it ("monetize \"" ++ x ++ "\"") $
        Ch.lessThanThou (L.listh x) `shouldBe` L.listh expected

  describe "words" $ do
    let xs =
          [ ("1000", "one thousand"),
            ("1001", "one thousand one"),
            ("1053", "one thousand fifty-three"),
            ("1153", "one thousand one hundred and fifty-three"),
            ("9020", "nine thousand twenty"),
            ("9999", "nine thousand nine hundred and ninety-nine"),
            ("999999", "nine hundred and ninety-nine thousand nine hundred and ninety-nine"),
            ("2000000", "two million"),
            ("2000003", "two million three")
          ]

    CM.forM_ xs $ \(x, expected) -> do
      it ("monetize \"" ++ x ++ "\"") $
        Ch.words (L.listh x) `shouldBe` L.listh expected

  describe "dollars" $ do
    it "empty" $
      Ch.dollars "0" `shouldBe` "zero dollars and zero cents"
    it "dollars '1'" $
      Ch.dollars "1" `shouldBe` "one dollar and zero cents"
    it "dollars '0.1'" $
      Ch.dollars "0.1" `shouldBe` "zero dollars and ten cents"
    it "dollars '1.'" $
      Ch.dollars "1." `shouldBe` "one dollar and zero cents"
    it "dollars '0.'" $
      Ch.dollars "0." `shouldBe` "zero dollars and zero cents"
    it "dollars '0.0'" $
      Ch.dollars "0.0" `shouldBe` "zero dollars and zero cents"
    it "dollars '.34'" $
      Ch.dollars ".34" `shouldBe` "zero dollars and thirty-four cents"
    it "dollars '0.3456789'" $
      Ch.dollars "0.3456789" `shouldBe` "zero dollars and thirty-four cents"
    it "dollars '1.0'" $
      Ch.dollars "1.0" `shouldBe` "one dollar and zero cents"
    it "dollars '1.01'" $
      Ch.dollars "1.01" `shouldBe` "one dollar and one cent"
    it "dollars 'a1a'" $
      Ch.dollars "a1a" `shouldBe` "one dollar and zero cents"
    it "dollars 'a1a.a0.7b'" $
      Ch.dollars "a1a.a0.7b" `shouldBe` "one dollar and seven cents"
    it "dollars '100'" $
      Ch.dollars "100" `shouldBe` "one hundred dollars and zero cents"
    it "dollars '100.0'" $
      Ch.dollars "100.0" `shouldBe` "one hundred dollars and zero cents"
    it "dollars '100.00'" $
      Ch.dollars "100.00" `shouldBe` "one hundred dollars and zero cents"
    it "dollars '100.00000'" $
      Ch.dollars "100.00000" `shouldBe` "one hundred dollars and zero cents"
    it "dollars '1000456.13'" $
      Ch.dollars "1000456.13" `shouldBe` "one million four hundred and fifty-six dollars and thirteen cents"
    it "dollars '1001456.13'" $
      Ch.dollars "1001456.13" `shouldBe` "one million one thousand four hundred and fifty-six dollars and thirteen cents"
    it "dollars '16000000456.13'" $
      Ch.dollars "16000000456.13" `shouldBe` "sixteen billion four hundred and fifty-six dollars and thirteen cents"
    it "dollars '100.45'" $
      Ch.dollars "100.45" `shouldBe` "one hundred dollars and forty-five cents"
    it "dollars '100.07'" $
      Ch.dollars "100.07" `shouldBe` "one hundred dollars and seven cents"
    it "dollars '9abc9def9ghi.jkl9mno'" $
      Ch.dollars "9abc9def9ghi.jkl9mno" `shouldBe` "nine hundred and ninety-nine dollars and ninety cents"
    it "dollars '12345.67'" $
      Ch.dollars "12345.67" `shouldBe` "twelve thousand three hundred and forty-five dollars and sixty-seven cents"
    it "dollars '456789123456789012345678901234567890123456789012345678901234567890.12'" $
      Ch.dollars "456789123456789012345678901234567890123456789012345678901234567890.12" `shouldBe` "four hundred and fifty-six vigintillion seven hundred and eighty-nine novemdecillion one hundred and twenty-three octodecillion four hundred and fifty-six septendecillion seven hundred and eighty-nine sexdecillion twelve quindecillion three hundred and forty-five quattuordecillion six hundred and seventy-eight tredecillion nine hundred and one duodecillion two hundred and thirty-four undecillion five hundred and sixty-seven decillion eight hundred and ninety nonillion one hundred and twenty-three octillion four hundred and fifty-six septillion seven hundred and eighty-nine sextillion twelve quintillion three hundred and forty-five quadrillion six hundred and seventy-eight trillion nine hundred and one billion two hundred and thirty-four million five hundred and sixty-seven thousand eight hundred and ninety dollars and twelve cents"
