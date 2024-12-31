module ListZipperSpec (spec) where

import qualified Applicative as A
-- import ExactlyOne (ExactlyOne (..))

import qualified Comonad as C
import qualified Extend as E
import qualified Functor as F
import List (List (..))
import qualified List as L
import ListZipper (ListZipper (..), MaybeListZipper (..))
import qualified ListZipper as LZ
import Optional (Optional (..))
import Property ()
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Traversable as T

optional :: b -> (a -> b) -> Optional a -> b
optional e _ Empty = e
optional _ f (Full a) = f a

spec :: Spec
spec = do
  it "ListZipper (<$>)" $
    (+ 1) F.<$> LZ.zipper [3, 2, 1] 4 [5, 6, 7] `shouldBe` LZ.zipper [4, 3, 2] 5 [6, 7, 8]

  it "MaybeListZipper (<$>)" $
    (+ 1) F.<$> LZ.isZ (LZ.zipper [3, 2, 1] 4 [5, 6, 7]) `shouldBe` LZ.isZ (LZ.zipper [4, 3, 2] 5 [6, 7, 8])

  describe "toList" $ do
    it "Optional empty list" $
      LZ.toList F.<$> Empty `shouldBe` (Empty :: Optional (List Int))
    it "empty left" $
      LZ.toList (LZ.zipper [] 1 [2, 3, 4]) `shouldBe` (1 :. 2 :. 3 :. 4 :. Nil)
    it "lefts and rights" $
      LZ.toList (LZ.zipper [3, 2, 1] 4 [5, 6, 7]) `shouldBe` (1 :. 2 :. 3 :. 4 :. 5 :. 6 :. 7 :. Nil)

  describe "fromList" $ do
    it "non-empty" $ LZ.fromList (1 :. 2 :. 3 :. Nil) `shouldBe` LZ.isZ (LZ.zipper [] 1 [2, 3])
    it "empty" $ LZ.fromList Nil `shouldBe` (LZ.isNotZ :: MaybeListZipper Integer)
    prop "round trip" $ \xs ->
      LZ.toListZ (LZ.fromList xs) `shouldBe` (xs :: List Integer)

  describe "toOptional" $ do
    prop "empty" $ \xs ->
      L.isEmpty xs `shouldBe` (LZ.toOptional (LZ.fromList xs) == (Empty :: Optional (ListZipper Integer)))

  describe "withFocus" $ do
    it "empty left" $
      LZ.withFocus (+ 1) (LZ.zipper [] 0 [1]) `shouldBe` LZ.zipper [] 1 [1]
    it "left and right" $
      LZ.withFocus (+ 1) (LZ.zipper [1, 0] 2 [3, 4]) `shouldBe` LZ.zipper [1, 0] 3 [3, 4]

  describe "setFocus" $ do
    it "empty left" $
      LZ.setFocus 1 (LZ.zipper [] 0 [1]) `shouldBe` LZ.zipper [] 1 [1]
    it "left and right" $
      LZ.setFocus 1 (LZ.zipper [1, 0] 2 [3, 4]) `shouldBe` LZ.zipper [1, 0] 1 [3, 4]

  describe "hasLeft" $ do
    it "left and right" $ LZ.hasLeft (LZ.zipper [1, 0] 2 [3, 4]) `shouldBe` True
    it "empty left" $ LZ.hasLeft (LZ.zipper [] 0 [1, 2]) `shouldBe` False

  describe "hasRight" $ do
    it "left and right" $ LZ.hasRight (LZ.zipper [1, 0] 2 [3, 4]) `shouldBe` True
    it "empty right" $ LZ.hasRight (LZ.zipper [1, 0] 2 []) `shouldBe` False

  describe "findLeft" $ do
    prop "missing element returns isNotZ" $ \xs p ->
      LZ.findLeft (const p) LZ.-<< LZ.fromList xs `shouldBe` (LZ.isNotZ :: MaybeListZipper Integer)
    it "found in left" $
      LZ.findLeft (== 1) (LZ.zipper [2, 1] 3 [4, 5]) `shouldBe` LZ.isZ (LZ.zipper [] 1 [2, 3, 4, 5])
    it "not found" $
      LZ.findLeft (== 6) (LZ.zipper [2, 1] 3 [4, 5]) `shouldBe` LZ.isNotZ
    it "one match in left" $
      LZ.findLeft (== 1) (LZ.zipper [2, 1] 1 [4, 5]) `shouldBe` LZ.isZ (LZ.zipper [] 1 [2, 1, 4, 5])
    it "multiple matches in left" $
      LZ.findLeft (== 1) (LZ.zipper [1, 2, 1] 3 [4, 5]) `shouldBe` LZ.isZ (LZ.zipper [2, 1] 1 [3, 4, 5])
    it "elements shifted to right correctly" $
      LZ.findLeft (== 1) (LZ.zipper [3, 4, 1, 5] 9 [2, 7]) `shouldBe` LZ.isZ (LZ.zipper [5] 1 [4, 3, 9, 2, 7])

  describe "findRight" $ do
    prop "missing element returns isNotZ" $ \xs ->
      LZ.findRight (const False) LZ.-<< LZ.fromList xs `shouldBe` (LZ.isNotZ :: MaybeListZipper Integer)
    it "found in right" $
      LZ.findRight (== 5) (LZ.zipper [2, 1] 3 [4, 5]) `shouldBe` LZ.isZ (LZ.zipper [4, 3, 2, 1] 5 [])
    it "not found" $
      LZ.findRight (== 6) (LZ.zipper [2, 1] 3 [4, 5]) `shouldBe` LZ.isNotZ
    it "one match in right" $
      LZ.findRight (== 1) (LZ.zipper [2, 3] 1 [4, 5, 1]) `shouldBe` LZ.isZ (LZ.zipper [5, 4, 1, 2, 3] 1 [])
    it "multiple matches in right" $
      LZ.findRight (== 1) (LZ.zipper [2, 3] 1 [1, 4, 5, 1]) `shouldBe` LZ.isZ (LZ.zipper [1, 2, 3] 1 [4, 5, 1])

  describe "moveLeftLoop" $ do
    it "with left" $
      LZ.moveLeftLoop (LZ.zipper [3, 2, 1] 4 [5, 6, 7]) `shouldBe` LZ.zipper [2, 1] 3 [4, 5, 6, 7]
    it "empty left" $
      LZ.moveLeftLoop (LZ.zipper [] 1 [2, 3, 4]) `shouldBe` LZ.zipper [3, 2, 1] 4 []

  describe "moveRightLoop" $ do
    it "with right" $
      LZ.moveRightLoop (LZ.zipper [3, 2, 1] 4 [5, 6, 7]) `shouldBe` LZ.zipper [4, 3, 2, 1] 5 [6, 7]
    it "empty right" $
      LZ.moveRightLoop (LZ.zipper [3, 2, 1] 4 []) `shouldBe` LZ.zipper [] 1 [2, 3, 4]

  describe "moveLeft" $ do
    it "with left" $
      LZ.moveLeft (LZ.zipper [3, 2, 1] 4 [5, 6, 7]) `shouldBe` LZ.isZ (LZ.zipper [2, 1] 3 [4, 5, 6, 7])
    it "empty left" $
      LZ.moveLeft (LZ.zipper [] 1 [2, 3, 4]) `shouldBe` LZ.isNotZ

  describe "moveRight" $ do
    it "with right" $
      LZ.moveRight (LZ.zipper [3, 2, 1] 4 [5, 6, 7]) `shouldBe` LZ.isZ (LZ.zipper [4, 3, 2, 1] 5 [6, 7])
    it "empty right" $
      LZ.moveRight (LZ.zipper [3, 2, 1] 4 []) `shouldBe` LZ.isNotZ

  describe "swapLeft" $ do
    it "with left" $
      LZ.swapLeft (LZ.zipper [3, 2, 1] 4 [5, 6, 7]) `shouldBe` LZ.isZ (LZ.zipper [4, 2, 1] 3 [5, 6, 7])
    it "empty left" $
      LZ.swapLeft (LZ.zipper [] 1 [2, 3, 4]) `shouldBe` LZ.isNotZ

  describe "swapRight" $ do
    it "with right" $
      LZ.swapRight (LZ.zipper [3, 2, 1] 4 [5, 6, 7]) `shouldBe` LZ.isZ (LZ.zipper [3, 2, 1] 5 [4, 6, 7])
    it "empty right" $
      LZ.swapRight (LZ.zipper [3, 2, 1] 4 []) `shouldBe` LZ.isNotZ

  describe "dropLeft" $ do
    it "with left" $
      LZ.dropLefts (LZ.zipper [3, 2, 1] 4 [5, 6, 7]) `shouldBe` LZ.zipper [] 4 [5, 6, 7]
    it "empty left" $
      LZ.dropLefts (LZ.zipper [] 1 [2, 3, 4]) `shouldBe` LZ.zipper [] 1 [2, 3, 4]
    prop "dropLefts empties left of zipper" $ \lz@(ListZipper _ x r) ->
      LZ.dropLefts lz `shouldBe` (ListZipper Nil x r :: ListZipper Integer)

  describe "dropRights" $ do
    it "with right" $
      LZ.dropRights (LZ.zipper [3, 2, 1] 4 [5, 6, 7]) `shouldBe` LZ.zipper [3, 2, 1] 4 []
    it "empty right" $
      LZ.dropRights (LZ.zipper [3, 2, 1] 4 []) `shouldBe` LZ.zipper [3, 2, 1] 4 []
    prop "dropRights empties right of zipper" $ \lz@(ListZipper l x _) ->
      LZ.dropRights lz `shouldBe` (ListZipper l x Nil :: ListZipper Integer)

  describe "moveLeftN" $ do
    it "positive moves" $
      LZ.moveLeftN 2 (LZ.zipper [2, 1, 0] 3 [4, 5, 6]) `shouldBe` LZ.isZ (LZ.zipper [0] 1 [2, 3, 4, 5, 6])
    it "negative moves" $
      LZ.moveLeftN (-1) (LZ.zipper [2, 1, 0] 3 [4, 5, 6]) `shouldBe` LZ.isZ (LZ.zipper [3, 2, 1, 0] 4 [5, 6])

  describe "moveRightN" $ do
    it "positive moves" $
      LZ.moveRightN 1 (LZ.zipper [2, 1, 0] 3 [4, 5, 6]) `shouldBe` LZ.isZ (LZ.zipper [3, 2, 1, 0] 4 [5, 6])
    it "negative moves" $
      LZ.moveRightN (-1) (LZ.zipper [2, 1, 0] 3 [4, 5, 6]) `shouldBe` LZ.isZ (LZ.zipper [1, 0] 2 [3, 4, 5, 6])

  describe "LZ.moveLeftN'" $ do
    it "positive - out of bounds both sides" $
      LZ.moveLeftN' 4 (LZ.zipper [3, 2, 1] 4 [5, 6, 7]) `shouldBe` Left 3
    it "positive in range" $
      LZ.moveLeftN' 1 (LZ.zipper [3, 2, 1] 4 [5, 6, 7]) `shouldBe` Right (LZ.zipper [2, 1] 3 [4, 5, 6, 7])
    prop "moving zero is `Right . id`" $ \lz ->
      LZ.moveLeftN' 0 lz `shouldBe` (Right lz :: Either Int (ListZipper Integer))
    it "negative in range" $
      LZ.moveLeftN' (-2) (LZ.zipper [3, 2, 1] 4 [5, 6, 7]) `shouldBe` Right (LZ.zipper [5, 4, 3, 2, 1] 6 [7])
    it "negative out of bounds" $
      LZ.moveLeftN' (-4) (LZ.zipper [3, 2, 1] 4 [5, 6, 7]) `shouldBe` Left 3
    it "positive - out of bounds on left only" $
      LZ.moveLeftN' 4 (LZ.zipper [3, 2, 1] 4 [5, 6, 7, 8, 9]) `shouldBe` Left 3
    it "negative - out of bounds on right only" $
      LZ.moveLeftN' (-4) (LZ.zipper [5, 4, 3, 2, 1] 6 [7, 8, 9]) `shouldBe` Left 3

  describe "moveRightN'" $ do
    it "positive - out of bounds both sides" $
      LZ.moveRightN' 4 (LZ.zipper [3, 2, 1] 4 [5, 6, 7]) `shouldBe` Left 3
    it "positive in range" $
      LZ.moveRightN' 1 (LZ.zipper [3, 2, 1] 4 [5, 6, 7]) `shouldBe` Right (LZ.zipper [4, 3, 2, 1] 5 [6, 7])
    prop "moving zero is `Right . id`" $ \lz ->
      LZ.moveRightN' 0 lz `shouldBe` (Right lz :: Either Int (ListZipper Integer))
    it "negative in range" $
      LZ.moveRightN' (-2) (LZ.zipper [3, 2, 1] 4 [5, 6, 7]) `shouldBe` Right (LZ.zipper [1] 2 [3, 4, 5, 6, 7])
    it "negative - out of bounds both sides" $
      LZ.moveRightN' (-4) (LZ.zipper [3, 2, 1] 4 [5, 6, 7]) `shouldBe` Left 3

  describe "nth" $ do
    it "have 1" $ LZ.nth 1 (LZ.zipper [3, 2, 1] 4 [5, 6, 7]) `shouldBe` LZ.isZ (LZ.zipper [1] 2 [3, 4, 5, 6, 7])
    it "have 5" $ LZ.nth 5 (LZ.zipper [3, 2, 1] 4 [5, 6, 7]) `shouldBe` LZ.isZ (LZ.zipper [5, 4, 3, 2, 1] 6 [7])
    it "missing 8" $ LZ.nth 8 (LZ.zipper [3, 2, 1] 4 [5, 6, 7]) `shouldBe` LZ.isNotZ

  describe "index" $ do
    it "index works" $ LZ.index (LZ.zipper [3, 2, 1] 4 [5, 6, 7]) `shouldBe` 3
    prop "Always returns the index on a valid zipper" $ \z i ->
      optional True (\z' -> LZ.index (z' :: ListZipper Integer) == i) (LZ.toOptional (LZ.nth i z)) `shouldBe` True

  describe "end" $ do
    it "end" $ LZ.end (LZ.zipper [3, 2, 1] 4 [5, 6, 7]) `shouldBe` LZ.zipper [6, 5, 4, 3, 2, 1] 7 []
    prop "end never changes the zipper's contents" $ \z ->
      LZ.toList z `shouldBe` (LZ.toList (LZ.end z) :: List Integer)
    prop "never have rights after calling end" $ \z ->
      LZ.rights (LZ.end z) `shouldBe` (Nil :: List Integer)

  describe "start" $ do
    it "start" $ LZ.start (LZ.zipper [3, 2, 1] 4 [5, 6, 7]) `shouldBe` LZ.zipper [] 1 [2, 3, 4, 5, 6, 7]
    prop "start never changes the zipper's contents" $ \z ->
      LZ.toList z `shouldBe` (LZ.toList (LZ.start z) :: List Integer)
    prop "never have lefts after calling start" $ \z ->
      LZ.lefts (LZ.start z) `shouldBe` (Nil :: List Integer)

  describe "deletePullLeft" $ do
    it "non-empty lefts" $ LZ.deletePullLeft (LZ.zipper [3, 2, 1] 4 [5, 6, 7]) `shouldBe` LZ.isZ (LZ.zipper [2, 1] 3 [5, 6, 7])
    it "empty lefts" $ LZ.deletePullLeft (LZ.zipper [] 1 [2, 3, 4]) `shouldBe` LZ.isNotZ

  describe "deletePullRight" $ do
    it "non-empty rights" $ LZ.deletePullRight (LZ.zipper [3, 2, 1] 4 [5, 6, 7]) `shouldBe` LZ.isZ (LZ.zipper [3, 2, 1] 5 [6, 7])
    it "empty rights" $ LZ.deletePullRight (LZ.zipper [3, 2, 1] 4 []) `shouldBe` LZ.isNotZ

  describe "insertPushLeft" $ do
    it "non-empty lefts" $
      LZ.insertPushLeft 15 (LZ.zipper [3, 2, 1] 4 [5, 6, 7]) `shouldBe` LZ.zipper [4, 3, 2, 1] 15 [5, 6, 7]
    it "empty lefts" $
      LZ.insertPushLeft 15 (LZ.zipper [] 1 [2, 3, 4]) `shouldBe` LZ.zipper [1] 15 [2, 3, 4]
    prop "deletePullLeft . insertPushLeft == id" $ \z i ->
      optional
        False
        (== (z :: ListZipper Integer))
        (LZ.toOptional (LZ.deletePullLeft (LZ.insertPushLeft i z)))
        `shouldBe` True

  describe "insertPushRight" $ do
    it "non-empty rights" $
      LZ.insertPushRight 15 (LZ.zipper [3, 2, 1] 4 [5, 6, 7]) `shouldBe` LZ.zipper [3, 2, 1] 15 [4, 5, 6, 7]
    it "empty rights" $
      LZ.insertPushRight 15 (LZ.zipper [3, 2, 1] 4 []) `shouldBe` LZ.zipper [3, 2, 1] 15 [4]
    prop "deletePullRight . insertPushRight == id" $ \z i ->
      optional
        False
        (== (z :: ListZipper Integer))
        (LZ.toOptional (LZ.deletePullRight (LZ.insertPushRight i z)))
        `shouldBe` True

  describe "Applicative" $ do
    prop "pure produces infinite lefts" $ \a n ->
      (L.all . (==) A.<*> L.take (getPositive (n :: Positive Int)) . LZ.lefts . A.pure) (a :: List Integer) `shouldBe` True
    prop "pure produces infinite rights" $ \a n ->
      (L.all . (==) A.<*> L.take (getPositive (n :: Positive Int)) . LZ.rights . A.pure) (a :: List Integer) `shouldBe` True
    it "<*> applies functions to corresponding elements in zipper" $
      LZ.zipper [(+ 2), (+ 10)] (* 2) [(* 3), (4 *), (5 +)] A.<*> LZ.zipper [3, 2, 1] 4 [5, 6, 7] `shouldBe` LZ.zipper [5, 12] 8 [15, 24, 12]

  describe "Applicative (MaybeListZipper)" $ do
    let is (MLZ (Full z)) = z
        is _ = error "MaybeListZipper's Applicative instances is busted"
        notZ = LZ.isNotZ :: MaybeListZipper Integer

    prop "pure produces infinite lefts" $ \a n ->
      (L.all . (==) A.<*> L.take (getPositive (n :: Positive Int)) . LZ.lefts . is . A.pure) (a :: List Integer) `shouldBe` True
    prop "pure produces infinite rights" $ \a n ->
      (L.all . (==) A.<*> L.take (getPositive (n :: Positive Int)) . LZ.rights . is . A.pure) (a :: List Integer) `shouldBe` True
    it "isZ <*> isZ" $
      let z = LZ.isZ (LZ.zipper [(+ 2), (+ 10)] (* 2) [(* 3), (4 *), (5 +)]) A.<*> LZ.isZ (LZ.zipper [3, 2, 1] 4 [5, 6, 7])
       in z `shouldBe` LZ.isZ (LZ.zipper [5, 12] 8 [15, 24, 12])
    prop "isNotZ <*> isZ" $
      let fs = (LZ.isNotZ :: MaybeListZipper (Integer -> Integer))
       in \z -> (fs A.<*> LZ.isZ z) `shouldBe` LZ.isNotZ
    prop "isZ <*> isNotZ" $
      let a = (LZ.isNotZ :: MaybeListZipper Integer)
       in \z -> (LZ.isZ (getBlind (z :: Blind (ListZipper (Integer -> Integer)))) A.<*> a) `shouldBe` (LZ.isNotZ :: MaybeListZipper Integer)
    it "isNotZ <*> isNotZ" $
      LZ.isNotZ A.<*> LZ.isNotZ `shouldBe` notZ

  describe "Extend" $ do
    it "zipper o' zippers" $
      let z = LZ.zipper [2, 1] 3 [4, 5]
          l = [LZ.zipper [1] 2 [3, 4, 5], LZ.zipper [] 1 [2, 3, 4, 5]]
          r = [LZ.zipper [3, 2, 1] 4 [5], LZ.zipper [4, 3, 2, 1] 5 []]
       in (id E.<<= z) `shouldBe` LZ.zipper l z r

  describe "Extend (MaybeListZipper)" $ do
    it "isNotZ" $ (id E.<<= LZ.isNotZ) `shouldBe` (LZ.isNotZ :: MaybeListZipper (MaybeListZipper Integer))
    it "isZ" $
      let z = LZ.isZ (LZ.zipper [2, 1] 3 [4, 5])
          l = LZ.isZ <$> [LZ.zipper [1] 2 [3, 4, 5], LZ.zipper [] 1 [2, 3, 4, 5]]
          r = LZ.isZ <$> [LZ.zipper [3, 2, 1] 4 [5], LZ.zipper [4, 3, 2, 1] 5 []]
       in (id E.<<= z) `shouldBe` LZ.isZ (LZ.zipper l z r)

  describe "Comonad" $ do
    it "copure" $ C.copure (LZ.zipper [2, 1] 3 [4, 5]) `shouldBe` 3

  describe "Traversable" $ do
    prop "All Full" $ \z ->
      T.traverse id (Full F.<$> z) `shouldBe` Full (z :: ListZipper Integer)
    it "One Empty" $
      T.traverse id (LZ.zipper [Full 1, Full 2, Full 3] (Full 4) [Empty, Full 6, Full 7]) `shouldBe` Empty
    it "Correct Order" $
      T.traverse id (LZ.zipper [error "traversing left values in wrong order", Empty] (error "traversing focus before left values") [Full 5, Full 6, Full 7]) `shouldBe` Empty

  describe "Traversable (MaybeListZipper)" $ do
    it "isNotZ" $ T.traverse id LZ.isNotZ `shouldBe` (Full LZ.isNotZ :: Optional (MaybeListZipper Integer))
    prop "isZ Full" $ \z ->
      T.traverse id (Full F.<$> LZ.isZ z) `shouldBe` Full (LZ.isZ (z :: ListZipper Integer))
