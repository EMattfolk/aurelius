{-# LANGUAGE NoImplicitPrelude #-}

module Parser2
  ( simplify,
    AST,
    Statement (..),
    Identifier (..),
    Symbol (..),
    Expression (..),
  )
where

import Data.Char (isSpace)
import Data.List (uncons)
import qualified Parser
import Std
import Text.Parsec (many1, optionMaybe)
import Text.Parsec.Char (char)
import Text.Parsec.Combinator (manyTill)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Pos (updatePosChar)
import Text.Parsec.Prim (Parsec, lookAhead, many, runParser, tokenPrim, try, unexpected, (<?>))

satisfy :: (Parser.Term -> Bool) -> TermParser Parser.Term
satisfy f =
  let test x =
        if f x
          then Just x
          else Nothing
   in satisfyMap test

satisfyMap :: (Parser.Term -> Maybe a) -> TermParser a
satisfyMap f =
  let nextPos pos x xs = pos -- TODO
   in tokenPrim show nextPos f

satisfyM :: (Parser.Term -> Maybe a) -> TermParser a
satisfyM f =
  let nextPos pos x xs = pos -- TODO
   in tokenPrim show nextPos f

-- AST = statement...
-- statement = definition | ...
-- definition = identifier identifier... '=' expression
-- expression = int | call
-- call = identifier identifier...
-- identifier = [a-z][a-zA-Z0-9]+
-- int = [1-9][0-9]+

type Identifier = Parser.Identifier

type Symbol = Parser.Symbol

type AST = [Statement]

data Statement
  = Definition Identifier [Identifier] Expression
  deriving (Show)

data Expression
  = Int Integer
  | Variable Identifier
  | BinOp Symbol Expression Expression
  | Call Expression [Expression]
  | Parenthesis Expression
  deriving (Show)

type TermParser = Parsec [Parser.Term] ()

statement :: Parser.Statement -> Statement
statement s =
  case s of
    Parser.Definition name args terms ->
      Definition name args (expression terms)

expression :: [Parser.Term] -> Expression
expression terms =
  let toExpr t = case t of
        Parser.Int i -> Int i
        Parser.Variable i -> Variable i
        Parser.Call t ts -> Call (toExpr t) (toExpr <$> ts)
        Parser.Parenthesis t -> Parenthesis (expression t)
      asSymbol t = case t of
        Parser.BinOp s -> s
   in case terms of
        [t] -> toExpr t
        left : op : ts -> BinOp (asSymbol op) (toExpr left) (expression ts)

simplify :: Parser.AST -> AST
simplify ast = statement <$> ast
