{-# LANGUAGE NoImplicitPrelude #-}

module Compiler (compile) where

import Control.Monad.State (evalState)
import Control.Monad.State.Lazy (State, get, modify, runState)
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

type Indent = State String

indent :: Indent ()
indent = modify (<> "  ")

dedent :: Indent ()
dedent = modify (drop 2)

withIndent :: String -> Indent String
withIndent s = get <#> (<> s)

statement :: Statement -> String
statement (Definition (Identifier name) args v) =
  let inner as = case as of
        (Identifier arg) : xs ->
          do
            indent
            bodyStart <- withIndent "return "
            innerBody <- inner xs
            dedent
            end <- withIndent "end"
            return ("function(" <> arg <> ")\n" <> bodyStart <> innerBody <> ";\n" <> end)
        [] ->
          return (value v)
   in name <> " = " <> evalState (inner args) "" <> ";"

value :: Value -> String
value (Int v) = show v

compile :: AST -> String
compile ast =
  case ast of
    x : xs -> statement x <> "\n\n" <> compile xs
    [] -> ""
