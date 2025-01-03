{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module FastAnagrams where

import Core
import qualified Data.Char as Ch
import qualified Data.Function as Fn
import qualified Data.Set as S
import qualified Functor as F
import List (Chars, FilePath, List)
import qualified List as L

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
-- on a Mac - run this with:
-- > fastAnagrams "Tony" "/usr/share/dict/words"
fastAnagrams :: Chars -> FilePath -> IO (List Chars)
fastAnagrams s filename =
  L.filter isMember . L.lines F.<$> content
  where
    content = L.readFile filename
    anagrams = L.foldLeft (flip (S.insert . NoCaseString)) S.empty (L.permutations s)
    isMember = flip S.member anagrams . NoCaseString

newtype NoCaseString
  = NoCaseString
      Chars

ncString :: NoCaseString -> Chars
ncString (NoCaseString s) = s

instance Eq NoCaseString where
  (==) = (==) `on` L.map Ch.toLower . ncString

instance Show NoCaseString where
  show = show . ncString

-- Instance of Ord is required to insert in a Set.
instance Ord NoCaseString where
  compare = Fn.on compare ncString
