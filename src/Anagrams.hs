{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Anagrams where

import Core
import qualified Data.Char as Ch
import qualified Data.Function as Fn
import qualified Functor as F
import List (Chars, FilePath, List (..))
import qualified List as L

{-

Functions you will need
--
\* fmap :: (a -> b) -> IO a -> IO b
\* readFile :: FilePath -> IO Str
\* lines :: Str -> [Str]
\* permutations :: [a] -> [[a]]
\* intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
\* toLower :: Char -> Char

Functions that might help
-
\* on :: (b -> b -> c) -> (a -> b) -> a -> a -> c

-}

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
anagrams :: Chars -> FilePath -> IO (List Chars)
anagrams s filename =
  L.intersectBy equalIgnoringCase (L.permutations s) . L.lines F.<$> L.readFile filename

-- Compare two strings for equality, ignoring case
equalIgnoringCase :: Chars -> Chars -> Bool
equalIgnoringCase = Fn.on (==) (Ch.toLower F.<$>)
