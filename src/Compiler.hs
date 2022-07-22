{-# LANGUAGE NoImplicitPrelude #-}

module Compiler (compile) where

import Control.Monad.State (evalState)
import Control.Monad.State.Lazy (State, get, modify, runState)
import Data.Foldable (fold)
import Data.List (intersperse)
import Parser
  ( Identifier (..),
    Symbol (..),
  )
import Parser2
  ( AST,
    Expression (..),
    Statement (..),
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

operatorName :: Symbol -> String
operatorName (Symbol s) = case s of
  "+" -> "add"
  "*" -> "mul"
  "-" -> "sub"
  "|>" -> "applyFlipped"
  ">>>" -> "composeFlipped"
  _ -> ""

expression :: Expression -> String
expression val =
  case val of
    Int v -> show v
    Variable (Identifier v) -> v
    BinOp op left right -> operatorName op <> "(" <> expression left <> ", " <> expression right <> ")"
    Call fn args -> expression fn <> foldMap (\v -> "(" <> expression v <> ")") args
    Parenthesis expr -> "(" <> expression expr <> ")"

compile :: AST -> String
compile ast =
  let prelude =
        "add = function(x, y) return x + y end\n"
          <> "mul = function(x, y) return x * y end\n"
          <> "sub = function(x, y) return x - y end\n"
          <> "applyFlipped = function(x, f) return f(x) end\n"
          <> "composeFlipped = function(g, f) return function(x) return f(g(x)) end end\n\n"
      compileStatements statements = case statements of
        x : xs -> statement x <> "\n\n" <> compileStatements xs
        [] -> ""
      postlude = "print(main);"
   in prelude <> compileStatements ast <> postlude
