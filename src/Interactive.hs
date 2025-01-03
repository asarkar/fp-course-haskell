{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Interactive where

import qualified Applicative as A
import Core
import qualified Data.Char as Ch
import Functor (Functor)
import qualified Functor as F
import List (Chars, List (..))
import qualified List as L
import Monad (Monad)
import qualified Monad as M
import Optional (Optional (..))
import qualified Traversable as T

-- | Eliminates any value over which a functor is defined.
vooid :: (Functor m) => m a -> m ()
vooid = (F.<$>) (const ())

-- | A version of @bind@ that ignores the result of the effect.
(>-) :: (Monad m) => m a -> m b -> m b
(>-) a = (M.>>=) a . const

-- | Runs an action until a result of that action satisfies a given predicate.
untilM ::
  (Monad m) =>
  -- | The predicate to satisfy to stop running the action.
  (a -> m Bool) ->
  -- | The action to run until the predicate satisfies.
  m a ->
  m a
untilM p a =
  a M.>>= \r ->
    p r M.>>= \q ->
      if q
        then
          A.pure r
        else
          untilM p a

-- | Example program that uses IO to echo back characters that are entered by the user.
echo :: IO ()
echo =
  vooid
    ( untilM
        ( \c ->
            if c == 'q'
              then
                L.putStrLn "Bye!"
                  >- A.pure True
              else
                A.pure False
        )
        ( L.putStr "Enter a character: "
            >- getChar
            M.>>= \c ->
              L.putStrLn ""
                >- L.putStrLn (c :. Nil)
                >- A.pure c
        )
    )

data Op
  = Op Char Chars (IO ()) -- keyboard entry, description, program

-- |
--
-- * Ask the user to enter a string to convert to upper-case.
--
-- * Convert the string to upper-case.
--
-- * Print the upper-cased string to standard output.
--
-- /Tip:/ @getLine :: IO String@ -- an IO action that reads a string from standard input.
--
-- /Tip:/ @toUpper :: Char -> Char@ -- (Data.Char) converts a character to upper-case.
--
-- /Tip:/ @putStr :: String -> IO ()@ -- Prints a string to standard output.
--
-- /Tip:/ @putStrLn :: String -> IO ()@ -- Prints a string and then a new line to standard output.
convertInteractive :: IO ()
convertInteractive = do
  _ <- L.putStr "Enter a string: "
  s <- L.getLine
  L.putStrLn (Ch.toUpper F.<$> s)

-- |
--
-- * Ask the user to enter a file name to reverse.
--
-- * Ask the user to enter a file name to write the reversed file to.
--
-- * Read the contents of the input file.
--
-- * Reverse the contents of the input file.
--
-- * Write the reversed contents to the output file.
--
-- /Tip:/ @getLine :: IO String@ -- an IO action that reads a string from standard input.
--
-- /Tip:/ @readFile :: FilePath -> IO String@ -- an IO action that reads contents of a file.
--
-- /Tip:/ @writeFile :: FilePath -> String -> IO ()@ -- writes a string to a file.
--
-- /Tip:/ @reverse :: [a] -> [a]@ -- reverses a list.
--
-- /Tip:/ @putStr :: String -> IO ()@ -- Prints a string to standard output.
--
-- /Tip:/ @putStrLn :: String -> IO ()@ -- Prints a string and then a new line to standard output.
reverseInteractive :: IO ()
reverseInteractive = do
  _ <- L.putStr "Enter the file name to reverse: "
  s <- L.getLine
  _ <- L.putStr "Enter the file name to write the reversed file to: "
  t <- L.getLine
  xs <- L.readFile s
  _ <- L.writeFile t (L.reverse xs)
  L.putStrLn ""

-- |
--
-- * Ask the user to enter a string to url-encode.
--
-- * Convert the string with a URL encoder.
--
-- * For simplicity, encoding is defined as:
--
-- * @' ' -> \"%20\"@
--
-- * @'\t' -> \"%09\"@
--
-- * @'\"' -> \"%22\"@
--
-- * @/anything else is unchanged/@
--
-- * Print the encoded URL to standard output.
--
-- /Tip:/ @putStr :: String -> IO ()@ -- Prints a string to standard output.
--
-- /Tip:/ @putStrLn :: String -> IO ()@ -- Prints a string and then a new line to standard output.
encodeInteractive :: IO ()
encodeInteractive = do
  _ <- L.putStr "Enter a string to encode: "
  s <- L.getLine
  L.putStrLn $ do
    c <- s
    case c of
      ' ' -> "%20"
      '\t' -> "%09"
      '\"' -> "%22"
      _ -> c :. Nil

interactive ::
  IO ()
interactive =
  let ops =
        ( Op 'c' "Convert a string to upper-case" convertInteractive
            :. Op 'r' "Reverse a file" reverseInteractive
            :. Op 'e' "Encode a URL" encodeInteractive
            :. Op 'q' "Quit" (A.pure ())
            :. Nil
        )
   in vooid
        ( untilM
            ( \c ->
                if c == 'q'
                  then
                    L.putStrLn "Bye!"
                      >- A.pure True
                  else
                    A.pure False
            )
            ( L.putStrLn "Select: "
                >- T.traverse
                  ( \(Op c s _) ->
                      L.putStr (c :. Nil)
                        >- L.putStr ". "
                        >- L.putStrLn s
                  )
                  ops
                >- getChar
                M.>>= \c ->
                  L.putStrLn ""
                    >- let o = L.find (\(Op c' _ _) -> c' == c) ops
                           r = case o of
                             Empty -> (L.putStrLn "Not a valid selection. Try again." >-)
                             Full (Op _ _ k) -> (k >-)
                        in r (A.pure c)
            )
        )
