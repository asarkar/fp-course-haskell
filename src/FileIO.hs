{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module FileIO where

import qualified Applicative as A
import Core
import qualified Functor as F
import List (Chars, FilePath, List (..))
import qualified List as L
import Monad ((>>=))

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: FilePath -> IO Chars  -- embedded '\n'
  lines :: Chars -> List Chars  -- splits by '\n'
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Tuple Functions that could help --

  fst :: (a, b) -> a
  snd :: (a, b) -> b
  (,) :: a -> b -> (a, b)

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Consideration --
  Try to avoid repetition. Factor out any common expressions.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

To test this module, load ghci in the root of the project directory, and do
    >> :main "data/files.txt"

Example output:

\$ ghci
GHCi, version ...
Loading package...
Loading ...
[ 1 of 28] Compiling (etc...
...
Ok, modules loaded: Course, etc...
>> :main "data/files.txt"
============ data/a.txt
the contents of a

============ data/b.txt
the contents of b

============ data/c.txt
the contents of c

-}

-- Given the file name, and file contents, print them.
-- Use @putStrLn@.
printFile :: FilePath -> Chars -> IO ()
printFile name content =
  L.putStrLn ("============ " L.++ name)
    A.*> L.putStrLn content

-- Given a list of (file name and file contents), print each.
-- Use @printFile@.
printFiles :: List (FilePath, Chars) -> IO ()
printFiles = L.foldLeft g (A.pure ())
  where
    {-
    g = (. uncurry printFile) . (*>)
    g acc (name, content)
    = ((. uncurry printFile) ((*>) acc)) (name, content)
    Operator section = partial application. Since the composition operator has been
    partially applied to the right, the output of (uncurry printFile) is input to
    ((*>) acc)), which emits the same.
    = (name, content) -> IO ()
    -}
    g = (. uncurry printFile) . (A.*>)

-- Given a file name, return (file name and file contents).
-- Use @readFile@.
getFile :: FilePath -> IO (FilePath, Chars)
{-
((->) t) is an Applicative, meaning it takes type t, and produces something.
(,) :: a -> b -> (a, b) fixes a :: FilePath and awaits b.
readFile :: FilePath -> IO Chars.
<$> maps over IO Chars using the partially-applied (FilePath,),
producing (FilePath, Chars).

Mind blown!!!
-}
getFile = A.lift2 (F.<$>) (,) L.readFile

-- Given a list of file names, return list of (file name and file contents).
-- Use @getFile@.
getFiles :: List FilePath -> IO (List (FilePath, Chars))
getFiles = A.sequence . (F.<$>) getFile

-- Given a file name, read it and for each line in that file, read and print contents of each.
-- Use @getFiles@, @lines@, and @printFiles@.
run :: FilePath -> IO ()
run filename = do
  content <- L.readFile filename
  results <- getFiles (L.lines content)
  printFiles results

-- /Tip:/ use @getArgs@ and @run@
main :: IO ()
main =
  L.getArgs >>= \case
    filename :. Nil -> run filename
    _ -> L.putStrLn "usage: stack runhaskell src/FileIO.hs data/files.txt"

----

-- Was there was some repetition in our solution?
-- ? `sequence . (<$>)`
-- ? `void . sequence . (<$>)`
-- Factor it out.
