{-# LANGUAGE NoImplicitPrelude #-}

module Compiler (compile) where

import Data.Foldable (fold)
import Data.List (intersperse)
import Parser
  ( AST,
    Identifier (..),
    Statement (..),
    Value (..),
    parse,
  )
import Std

statement :: Statement -> String
statement (Definition (Identifier name) args (Int v)) =
  name
    <> " = function("
    <> fold (intersperse "," (map (\(Identifier arg) -> arg) args))
    <> ") return "
    <> show v
    <> "; end;"

value :: Value -> String
value (Int v) = show v

compile :: AST -> String
compile ast =
  case ast of
    x : xs -> statement x <> "\n\n" <> compile xs
    [] -> ""
