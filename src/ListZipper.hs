{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS -Wno-incomplete-uni-patterns #-}

module ListZipper where

import Applicative (Applicative)
import qualified Applicative as A
import Comonad (Comonad, copure)
import Compose (Compose (..))
import Core
import Extend (Extend, (<<=))
import Functor (Functor)
import qualified Functor as F
import List (List (..))
import qualified List as L
import qualified Monad as M
import Optional (Optional (..))
import qualified Optional as O
import Traversable (Traversable, traverse)
import qualified Traversable as T

-- $setup
-- >>> import Test.QuickCheck
-- >>> import Data.Maybe(maybe)
-- >>> import Core
-- >>> import qualified Prelude as P
-- >>> let optional e _ Empty = e; optional _ f (Full a) = f a
-- >>> instance Arbitrary a => Arbitrary (Optional a) where arbitrary = P.fmap (maybe Empty Full) arbitrary
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap (P.foldr (:.) Nil :: ([a] -> List a)) arbitrary
-- >>> instance Arbitrary a => Arbitrary (ListZipper a) where arbitrary = do l <- arbitrary; x <- arbitrary; r <- arbitrary; P.return (ListZipper l x r)

-- A `ListZipper` is a focussed position, with a list of values to the left and to the right.
--
-- For example, taking the list [0,1,2,3,4,5,6], the moving focus to the third position, the zipper looks like:
-- ListZipper [2,1,0] 3 [4,5,6]
--
-- Supposing then we move left on this zipper:
-- ListZipper [1,0] 2 [3,4,5,6]
--
-- then suppose we add 17 to the focus of this zipper:
-- ListZipper [1,0] 19 [3,4,5,6]
data ListZipper a
  = ListZipper (List a) a (List a)
  deriving stock (Eq)

lefts :: ListZipper a -> List a
lefts (ListZipper l _ _) = l

rights :: ListZipper a -> List a
rights (ListZipper _ _ r) = r

-- A `MaybeListZipper` is a data structure that allows us to "fail" zipper operations.
-- e.g. Moving left when there are no values to the left.
newtype MaybeListZipper a
  = MLZ (Optional (ListZipper a))
  deriving stock (Eq)

isZ :: ListZipper a -> MaybeListZipper a
isZ = MLZ . Full

isNotZ :: MaybeListZipper a
isNotZ = MLZ Empty

-- | Implement the `Functor` instance for `ListZipper`.
--
-- >>> (+1) <$> (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2] >5< [6,7,8]
instance Functor ListZipper where
  (<$>) :: (a -> b) -> ListZipper a -> ListZipper b
  f <$> (ListZipper left a right) = ListZipper (f F.<$> left) (f a) (f F.<$> right)

-- | Implement the `Functor` instance for `MaybeListZipper`.
--
-- >>> (+1) <$> (MLZ (Full (zipper [3,2,1] 4 [5,6,7])))
-- [4,3,2] >5< [6,7,8]
instance Functor MaybeListZipper where
  (<$>) :: (a -> b) -> MaybeListZipper a -> MaybeListZipper b
  f <$> MLZ x = MLZ y
    where
      Compose y = f F.<$> Compose x

-- | Convert the given zipper back to a list.
--
-- >>> toList <$> toOptional (fromList Nil)
-- Empty
--
-- >>> toList (ListZipper Nil 1 (2:.3:.4:.Nil))
-- [1,2,3,4]
--
-- >>> toList (ListZipper (3:.2:.1:.Nil) 4 (5:.6:.7:.Nil))
-- [1,2,3,4,5,6,7]
toList :: ListZipper a -> List a
toList (ListZipper left a right) = L.reverse left L.++ a :. right

-- | Convert the given (maybe) zipper back to a list.
toListZ :: MaybeListZipper a -> List a
toListZ (MLZ Empty) = Nil
toListZ (MLZ (Full z)) = toList z

-- | Create a `MaybeListZipper` positioning the focus at the head.
--
-- ->>> fromList (1 :. 2 :. 3 :. Nil)
-- [] >1< [2,3]
--
-- >>> fromList Nil
-- ><
--
-- prop> \xs -> xs == toListZ (fromList xs)
fromList :: List a -> MaybeListZipper a
fromList Nil = isNotZ
fromList (x :. xs) = isZ $ ListZipper Nil x xs

-- | Retrieve the `ListZipper` from the `MaybeListZipper` if there is one.
--
-- prop> \xs -> isEmpty xs == (toOptional (fromList xs) == Empty)
--
-- prop> \z -> toOptional (fromOptional z) == z
toOptional :: MaybeListZipper a -> Optional (ListZipper a)
toOptional (MLZ Empty) = Empty
toOptional (MLZ z) = z

zipper :: [a] -> a -> [a] -> ListZipper a
zipper l x r = ListZipper (L.listh l) x (L.listh r)

fromOptional :: Optional (ListZipper a) -> MaybeListZipper a
fromOptional = MLZ

asZipper :: (ListZipper a -> ListZipper a) -> MaybeListZipper a -> MaybeListZipper a
asZipper f = asMaybeZipper (isZ . f)

(>$>) :: (ListZipper a -> ListZipper a) -> MaybeListZipper a -> MaybeListZipper a
(>$>) = asZipper

asMaybeZipper :: (ListZipper a -> MaybeListZipper a) -> MaybeListZipper a -> MaybeListZipper a
asMaybeZipper _ (MLZ Empty) = isNotZ
asMaybeZipper f (MLZ (Full z)) = f z

(-<<) :: (ListZipper a -> MaybeListZipper a) -> MaybeListZipper a -> MaybeListZipper a
(-<<) = asMaybeZipper

-- | Update the focus of the zipper with the given function on the current focus.
--
-- >>> withFocus (+1) (zipper [] 0 [1])
-- [] >1< [1]
--
-- >>> withFocus (+1) (zipper [1,0] 2 [3,4])
-- [1,0] >3< [3,4]
withFocus :: (a -> a) -> ListZipper a -> ListZipper a
withFocus f (ListZipper left a right) = ListZipper left (f a) right

-- | Set the focus of the zipper to the given value.
-- /Tip:/ Use `withFocus`.
--
-- >>> setFocus 1 (zipper [] 0 [1])
-- [] >1< [1]
--
-- >>> setFocus 1 (zipper [1,0] 2 [3,4])
-- [1,0] >1< [3,4]
setFocus :: a -> ListZipper a -> ListZipper a
setFocus = withFocus . const

-- A flipped infix alias for `setFocus`. This allows:
--
-- z .= "abc" -- sets the focus on the zipper z to the value "abc".
(.=) :: ListZipper a -> a -> ListZipper a
(.=) = flip setFocus

-- | Returns whether there are values to the left of focus.
--
-- >>> hasLeft (zipper [1,0] 2 [3,4])
-- True
--
-- >>> hasLeft (zipper [] 0 [1,2])
-- False
hasLeft :: ListZipper a -> Bool
hasLeft = not . L.isEmpty . lefts

-- | Returns whether there are values to the right of focus.
--
-- >>> hasRight (zipper [1,0] 2 [3,4])
-- True
--
-- >>> hasRight (zipper [1,0] 2 [])
-- False
hasRight :: ListZipper a -> Bool
hasRight = not . L.isEmpty . rights

-- | Seek to the left for a location matching a predicate, excluding the
-- focus.
--
-- /Tip:/ Use `break`
--
-- prop> \xs p -> findLeft (const p) -<< fromList xs == isNotZ
--
-- >>> findLeft (== 1) (zipper [2, 1] 3 [4, 5])
-- [] >1< [2,3,4,5]
--
-- >>> findLeft (== 6) (zipper [2, 1] 3 [4, 5])
-- ><
--
-- >>> findLeft (== 1) (zipper [2, 1] 1 [4, 5])
-- [] >1< [2,1,4,5]
--
-- >>> findLeft (== 1) (zipper [1, 2, 1] 3 [4, 5])
-- [2,1] >1< [3,4,5]
--
-- >>> findLeft (== 1) (zipper [3, 4, 1, 5] 9 [2, 7])
-- [5] >1< [4,3,9,2,7]
findLeft :: (a -> Bool) -> ListZipper a -> MaybeListZipper a
findLeft p (ListZipper left a right) =
  case L.break p left of
    (_, Nil) -> isNotZ
    (l, x :. xs) -> isZ $ ListZipper xs x (L.reverse l L.++ a :. right)

-- | Seek to the right for a location matching a predicate, excluding the
-- focus.
--
-- /Tip:/ Use `break`
--
-- prop> \xs -> findRight (const False) -<< fromList xs == isNotZ
--
-- >>> findRight (== 5) (zipper [2, 1] 3 [4, 5])
-- [4,3,2,1] >5< []
--
-- >>> findRight (== 6) (zipper [2, 1] 3 [4, 5])
-- ><
--
-- >>> findRight (== 1) (zipper [2, 3] 1 [4, 5, 1])
-- [5,4,1,2,3] >1< []
--
-- >>> findRight (== 1) (zipper [2, 3] 1 [1, 4, 5, 1])
-- [1,2,3] >1< [4,5,1]
findRight :: (a -> Bool) -> ListZipper a -> MaybeListZipper a
findRight p (ListZipper left a right) =
  case L.break p right of
    (_, Nil) -> isNotZ
    (l, x :. xs) -> isZ $ ListZipper (L.reverse l L.++ a :. left) x xs

-- | Move the zipper left, or if there are no elements to the left, go to the far right.
--
-- >>> moveLeftLoop (zipper [3,2,1] 4 [5,6,7])
-- [2,1] >3< [4,5,6,7]
--
-- >>> moveLeftLoop (zipper [] 1 [2,3,4])
-- [3,2,1] >4< []
moveLeftLoop :: ListZipper a -> ListZipper a
moveLeftLoop (ListZipper left a right) =
  case left of
    x :. xs -> ListZipper xs x (a :. right)
    _ -> let (y :. ys) = L.reverse (a :. right) in ListZipper ys y Nil

-- | Move the zipper right, or if there are no elements to the right, go to the far left.
--
-- >>> moveRightLoop (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2,1] >5< [6,7]
--
-- >>> moveRightLoop (zipper [3,2,1] 4 [])
-- [] >1< [2,3,4]
moveRightLoop :: ListZipper a -> ListZipper a
moveRightLoop (ListZipper left a right) =
  case right of
    x :. xs -> ListZipper (a :. left) x xs
    _ -> let (y :. ys) = L.reverse (a :. left) in ListZipper Nil y ys

-- | Move the zipper one position to the left.
--
-- >>> moveLeft (zipper [3,2,1] 4 [5,6,7])
-- [2,1] >3< [4,5,6,7]
--
-- >>> moveLeft (zipper [] 1 [2,3,4])
-- ><
moveLeft :: ListZipper a -> MaybeListZipper a
moveLeft (ListZipper left a right) =
  case left of
    x :. xs -> isZ $ ListZipper xs x (a :. right)
    _ -> isNotZ

-- | Move the zipper one position to the right.
--
-- >>> moveRight (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2,1] >5< [6,7]
--
-- >>> moveRight (zipper [3,2,1] 4 [])
-- ><
moveRight :: ListZipper a -> MaybeListZipper a
moveRight (ListZipper left a right) =
  case right of
    x :. xs -> isZ $ ListZipper (a :. left) x xs
    _ -> isNotZ

-- | Swap the current focus with the value to the left of focus.
--
-- >>> swapLeft (zipper [3,2,1] 4 [5,6,7])
-- [4,2,1] >3< [5,6,7]
--
-- >>> swapLeft (zipper [] 1 [2,3,4])
-- ><
swapLeft :: ListZipper a -> MaybeListZipper a
swapLeft (ListZipper left a right) =
  case left of
    x :. xs -> isZ $ ListZipper (a :. xs) x right
    _ -> isNotZ

-- | Swap the current focus with the value to the right of focus.
--
-- >>> swapRight (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >5< [4,6,7]
--
-- >>> swapRight (zipper [3,2,1] 4 [])
-- ><
swapRight :: ListZipper a -> MaybeListZipper a
swapRight (ListZipper left a right) =
  case right of
    x :. xs -> isZ $ ListZipper left x (a :. xs)
    _ -> isNotZ

-- | Drop all values to the left of the focus.
--
-- >>> dropLefts (zipper [3,2,1] 4 [5,6,7])
-- [] >4< [5,6,7]
--
-- >>> dropLefts (zipper [] 1 [2,3,4])
-- [] >1< [2,3,4]
--
-- prop> \l x r -> dropLefts (zipper l x r) == zipper [] x r
dropLefts :: ListZipper a -> ListZipper a
dropLefts (ListZipper _ a right) = ListZipper Nil a right

-- | Drop all values to the right of the focus.
--
-- >>> dropRights (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >4< []
--
-- >>> dropRights (zipper [3,2,1] 4 [])
-- [3,2,1] >4< []
--
-- prop> \l x r -> dropRights (zipper l x r) == zipper l x []
dropRights :: ListZipper a -> ListZipper a
dropRights (ListZipper left a _) = ListZipper left a Nil

-- | Move the focus left the given number of positions. If the value is negative, move right instead.
--
-- >>> moveLeftN 2 (zipper [2,1,0] 3 [4,5,6])
-- [0] >1< [2,3,4,5,6]
--
-- >>> moveLeftN (-1) $ zipper [2,1,0] 3 [4,5,6]
-- [3,2,1,0] >4< [5,6]
moveLeftN :: Int -> ListZipper a -> MaybeListZipper a
moveLeftN n z
  | n < 0 = moveRightN (-n) z
  | n == 0 = isZ z
  | L.isEmpty (lefts z) = isNotZ
  | otherwise = moveLeftN (n - 1) -<< moveLeft z

-- | Move the focus right the given number of positions. If the value is negative, move left instead.
--
-- >>> moveRightN 1 (zipper [2,1,0] 3 [4,5,6])
-- [3,2,1,0] >4< [5,6]
--
-- >>> moveRightN (-1) $ zipper [2,1,0] 3 [4,5,6]
-- [1,0] >2< [3,4,5,6]
moveRightN :: Int -> ListZipper a -> MaybeListZipper a
moveRightN n z
  | n < 0 = moveLeftN (-n) z
  | n == 0 = isZ z
  | L.isEmpty (rights z) = isNotZ
  | otherwise = moveRightN (n - 1) -<< moveRight z

-- | Move the focus left the given number of positions. If the value is negative, move right instead.
-- If the focus cannot be moved, the given number of times, return the value by which it can be moved instead.
--
-- >>> moveLeftN' 4 (zipper [3,2,1] 4 [5,6,7])
-- Left 3
--
-- >>> moveLeftN' 1 (zipper [3,2,1] 4 [5,6,7])
-- Right [2,1] >3< [4,5,6,7]
--
-- >>> moveLeftN' 0 (zipper [3,2,1] 4 [5,6,7])
-- Right [3,2,1] >4< [5,6,7]
--
-- >>> moveLeftN' (-2) (zipper [3,2,1] 4 [5,6,7])
-- Right [5,4,3,2,1] >6< [7]
--
-- >>> moveLeftN' (-4) (zipper [3,2,1] 4 [5,6,7])
-- Left 3
--
-- >>> moveLeftN' 4 (zipper [3,2,1] 4 [5,6,7,8,9])
-- Left 3
--
-- >>> moveLeftN' (-4) (zipper [5,4,3,2,1] 6 [7,8,9])
-- Left 3
--
-- >>> rights <$> moveLeftN' 1 (zipper [3,2,error "moveLeftN' not sufficiently lazy"] 4 [5,6,7])
-- Right [4,5,6,7]
moveLeftN' :: Int -> ListZipper a -> Either Int (ListZipper a)
moveLeftN' n z =
  case moveLeftN n z of
    MLZ Empty -> (Left . L.length . (if n >= 0 then lefts else rights)) z
    MLZ (Full z') -> Right z'

-- | Move the focus right the given number of positions. If the value is negative, move left instead.
-- If the focus cannot be moved, the given number of times, return the value by which it can be moved instead.
--
-- >>> moveRightN' 4 (zipper [3,2,1] 4 [5,6,7])
-- Left 3
--
-- >>> moveRightN' 1 (zipper [3,2,1] 4 [5,6,7])
-- Right [4,3,2,1] >5< [6,7]
--
-- >>> moveRightN' 0 (zipper [3,2,1] 4 [5,6,7])
-- Right [3,2,1] >4< [5,6,7]
--
-- >>> moveRightN' (-2) (zipper [3,2,1] 4 [5,6,7])
-- Right [1] >2< [3,4,5,6,7]
--
-- >>> moveRightN' (-4) (zipper [3,2,1] 4 [5,6,7])
-- Left 3
moveRightN' :: Int -> ListZipper a -> Either Int (ListZipper a)
moveRightN' n z =
  case moveRightN n z of
    MLZ Empty -> (Left . L.length . (if n >= 0 then rights else lefts)) z
    MLZ (Full z') -> Right z'

-- | Move the focus to the given absolute position in the zipper. Traverse the zipper only to the extent required.
--
-- >>> nth 1 (zipper [3,2,1] 4 [5,6,7])
-- [1] >2< [3,4,5,6,7]
--
-- >>> nth 5 (zipper [3,2,1] 4 [5,6,7])
-- [5,4,3,2,1] >6< [7]
--
-- >>> nth 8 (zipper [3,2,1] 4 [5,6,7])
-- ><
nth :: Int -> ListZipper a -> MaybeListZipper a
nth n z
  | n < 0 = isNotZ
  | otherwise =
      case moveLeftN' n z of
        -- Not enough elements in the left.
        Left k -> moveRightN (n - k) z
        -- The number of elements in the left of the returned
        -- zipper is the index to move from the end.
        Right (ListZipper l _ _) -> moveLeftN (L.length l) z

-- | Return the absolute position of the current focus in the zipper.
--
-- >>> index (zipper [3,2,1] 4 [5,6,7])
-- 3
--
-- prop> \i z -> optional True (\z' -> index z' == i) (toOptional (nth i z))
index :: ListZipper a -> Int
index = L.length . lefts

-- | Move the focus to the end of the zipper.
--
-- >>> end (zipper [3,2,1] 4 [5,6,7])
-- [6,5,4,3,2,1] >7< []
--
-- prop> \lz -> toList lz == toList (end lz)
--
-- prop> \lz -> rights (end lz) == Nil
end :: ListZipper a -> ListZipper a
end z =
  case moveRight z of
    MLZ Empty -> z
    MLZ (Full z') -> end z'

-- | Move the focus to the start of the zipper.
--
-- >>> start (zipper [3,2,1] 4 [5,6,7])
-- [] >1< [2,3,4,5,6,7]
--
-- prop> \lz -> toList lz == toList (start lz)
--
-- prop> \lz -> lefts (start lz) == Nil
start :: ListZipper a -> ListZipper a
start z = O.fullOr z z'
  where
    MLZ z' = nth 0 z

-- | Delete the current focus and pull the left values to take the empty position.
--
-- >>> deletePullLeft (zipper [3,2,1] 4 [5,6,7])
-- [2,1] >3< [5,6,7]
--
-- >>> deletePullLeft (zipper [] 1 [2,3,4])
-- ><
deletePullLeft :: ListZipper a -> MaybeListZipper a
deletePullLeft (ListZipper (x :. xs) _ right) = isZ $ ListZipper xs x right
deletePullLeft _ = isNotZ

-- | Delete the current focus and pull the right values to take the empty position.
--
-- >>> deletePullRight (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >5< [6,7]
--
-- >>> deletePullRight (zipper [3,2,1] 4 [])
-- ><
deletePullRight :: ListZipper a -> MaybeListZipper a
deletePullRight (ListZipper left _ (x :. xs)) = isZ $ ListZipper left x xs
deletePullRight _ = isNotZ

-- | Insert at the current focus and push the left values to make way for the new position.
--
-- >>> insertPushLeft 15 (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2,1] >15< [5,6,7]
--
-- >>> insertPushLeft 15 (zipper [] 1 [2,3,4])
-- [1] >15< [2,3,4]
--
-- prop> \i z -> optional False (==z) (toOptional (deletePullLeft (insertPushLeft i z)))
insertPushLeft :: a -> ListZipper a -> ListZipper a
insertPushLeft x (ListZipper left a right) = ListZipper (a :. left) x right

-- | Insert at the current focus and push the right values to make way for the new position.
--
-- >>> insertPushRight 15 (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >15< [4,5,6,7]
--
-- >>> insertPushRight 15 (zipper [3,2,1] 4 [])
-- [3,2,1] >15< [4]
--
-- prop> \i z -> optional False (==z) (toOptional (deletePullRight (insertPushRight i z)))
insertPushRight :: a -> ListZipper a -> ListZipper a
insertPushRight x (ListZipper left a right) = ListZipper left x (a :. right)

-- | Implement the `Applicative` instance for `ListZipper`.
-- `pure` produces an infinite list zipper (to both left and right).
-- (<*>) zips functions with values by function application.
--
-- prop> \n -> all . (==) <*> take n . lefts . pure
--
-- prop> \n -> all . (==) <*> take n . rights . pure
--
-- >>> zipper [(+2), (+10)] (*2) [(*3), (4*), (5+)] <*> zipper [3,2,1] 4 [5,6,7]
-- [5,12] >8< [15,24,12]
instance Applicative ListZipper where
  -- /Tip:/ Use @List#repeat@.
  pure :: a -> ListZipper a
  pure a = ListZipper (L.repeat a) a (L.repeat a)

  -- /Tip:/ Use `zipWith`
  (<*>) :: ListZipper (a -> b) -> ListZipper a -> ListZipper b
  (<*>) (ListZipper g f h) (ListZipper left a right) = ListZipper (combine g left) (f a) (combine h right)
    where
      combine = L.zipWith ($)

-- | Implement the `Applicative` instance for `MaybeListZipper`.
--
-- /Tip:/ Use @pure@ for `ListZipper`.
-- /Tip:/ Use `<*>` for `ListZipper`.
--
-- prop> \z n -> let is (MLZ (Full z)) = z in all . (==) <*> take n . lefts . is . pure
--
-- prop> \z n -> let is (MLZ (Full z)) = z in all . (==) <*> take n . rights . is . pure
--
-- >>> isZ (zipper [(+2), (+10)] (*2) [(*3), (4*), (5+)]) <*> isZ (zipper [3,2,1] 4 [5,6,7])
-- [5,12] >8< [15,24,12]
--
-- >>> isNotZ <*> isZ (zipper [3,2,1] 4 [5,6,7])
-- ><
--
-- >>> isZ (zipper [(+2), (+10)] (*2) [(*3), (4*), (5+)]) <*> isNotZ
-- ><
--
-- >>> isNotZ <*> isNotZ
-- ><
instance Applicative MaybeListZipper where
  pure :: a -> MaybeListZipper a
  pure = MLZ . Full . A.pure

  (<*>) :: MaybeListZipper (a -> b) -> MaybeListZipper a -> MaybeListZipper b
  (<*>) (MLZ f) (MLZ z) = MLZ z'
    where
      Compose z' = Compose f A.<*> Compose z

-- | Implement the `Extend` instance for `ListZipper`.
-- This implementation "visits" every possible zipper value derivable from a given zipper (i.e. all zippers to the left and right).
--
-- /Tip:/ Use @List#unfoldr@.
--
-- >>> id <<= (zipper [2,1] 3 [4,5])
-- [[1] >2< [3,4,5],[] >1< [2,3,4,5]] >[2,1] >3< [4,5]< [[3,2,1] >4< [5],[4,3,2,1] >5< []]
instance Extend ListZipper where
  (<<=) :: (ListZipper a -> b) -> ListZipper a -> ListZipper b
  (<<=) f z = ListZipper (go moveLeft) (f z) (go moveRight)
    where
      go g = L.unfoldr (move g) z
      -- Binds a function on the reader ((->) t).
      -- In other words, it applies a function to the first argument
      -- of the binary function on the left.
      -- >>> ((*) =<< (+10)) 7
      -- (7 + 10) * 7 = 119
      move g z' = ((,) M.=<< f) F.<$> z''
        where
          MLZ z'' = g z'

-- | Implement the `Extend` instance for `MaybeListZipper`.
-- This instance will use the `Extend` instance for `ListZipper`.
--
--
-- id <<= isNotZ
-- ><
--
-- >>> id <<= (isZ (zipper [2,1] 3 [4,5]))
-- [[1] >2< [3,4,5],[] >1< [2,3,4,5]] >[2,1] >3< [4,5]< [[3,2,1] >4< [5],[4,3,2,1] >5< []]
instance Extend MaybeListZipper where
  (<<=) :: (MaybeListZipper a -> b) -> MaybeListZipper a -> MaybeListZipper b
  (<<=) _ (MLZ Empty) = MLZ Empty
  (<<=) f (MLZ (Full z)) = MLZ (Full ((f . MLZ . Full) <<= z))

-- | Implement the `Comonad` instance for `ListZipper`.
-- This implementation returns the current focus of the zipper.
--
-- >>> copure (zipper [2,1] 3 [4,5])
-- 3
instance Comonad ListZipper where
  copure :: ListZipper a -> a
  copure (ListZipper _ a _) = a

-- | Implement the `Traversable` instance for `ListZipper`.
-- This implementation traverses a zipper from left to right while running
-- some `Applicative` effect through the zipper.
-- An effectful zipper is returned.
--
-- >>> traverse id (zipper [Full 1, Full 2, Full 3] (Full 4) [Full 5, Full 6, Full 7])
-- Full [1,2,3] >4< [5,6,7]
--
-- >>> traverse id (zipper [Full 1, Full 2, Full 3] (Full 4) [Empty, Full 6, Full 7])
-- Empty
--
-- >>> traverse id (zipper [error "traversing left values in wrong order", Empty] (error "traversing focus before left values") [Full 5, Full 6, Full 7])
-- Empty
instance Traversable ListZipper where
  traverse :: (Applicative k) => (a -> k b) -> ListZipper a -> k (ListZipper b)
  traverse f (ListZipper left a right) = ListZipper F.<$> l A.<*> f a A.<*> r
    where
      l = L.reverse F.<$> T.traverse f (L.reverse left)
      r = T.traverse f right

-- | Implement the `Traversable` instance for `MaybeListZipper`.
--
-- /Tip:/ Use `traverse` for `ListZipper`.
--
-- >>> traverse id isNotZ
-- ><
--
-- >>> traverse id (isZ (zipper [Full 1, Full 2, Full 3] (Full 4) [Full 5, Full 6, Full 7]))
-- Full [1,2,3] >4< [5,6,7]
instance Traversable MaybeListZipper where
  traverse :: (Applicative k) => (a -> k b) -> MaybeListZipper a -> k (MaybeListZipper b)
  traverse f (MLZ z) = MLZ F.<$> T.sequenceA (T.traverse f F.<$> z)

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance (Show a) => Show (ListZipper a) where
  show (ListZipper l x r) =
    L.stringconcat [show l, " >", show x, "< ", show r]

instance (Show a) => Show (MaybeListZipper a) where
  show (MLZ (Full z)) = show z
  show (MLZ Empty) = "><"
