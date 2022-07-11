{-# LANGUAGE NoImplicitPrelude #-}

module Compiler (compile) where

import Control.Monad.State (evalState)
import Control.Monad.State.Lazy (State, get, modify, runState)
import Data.Foldable (fold)
import Data.List (intersperse)
import Parser
  ( AST,
    Expression (..),
    Identifier (..),
    Statement (..),
    Symbol (..),
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
          return (expression v)
   in name <> " = " <> evalState (inner args) "" <> ";"

expression :: Expression -> String
expression val =
  case val of
    Int v -> show v
    Variable (Identifier v) -> v
    BinOp (Symbol op) left right -> "(" <> expression left <> " " <> op <> " " <> expression right <> ")"
    Call fn args -> expression fn <> foldMap (\v -> "(" <> expression v <> ")") args

compile :: AST -> String
compile ast =
  case ast of
    x : xs -> statement x <> "\n\n" <> compile xs
    [] -> "print(main);"
