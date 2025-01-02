{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module JsonValue where

import Core
import List (Chars, List)

type Assoc = List (Chars, JsonValue)

data JsonValue
  = JsonString Chars
  | JsonRational Rational
  | JsonObject Assoc
  | JsonArray (List JsonValue)
  | JsonTrue
  | JsonFalse
  | JsonNull
  deriving stock (Show, Eq)
