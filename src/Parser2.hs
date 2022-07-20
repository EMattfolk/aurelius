{-# LANGUAGE NoImplicitPrelude #-}

module Parser2
  ( parse,
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
  deriving (Show)

type TermParser = Parsec [Parser.Term] ()

statement :: Parser.Statement -> Either ParseError Statement
statement s =
  case s of
    Parser.Definition name args terms ->
      Definition name args <$> runParser expression () "Terms -> Expression" terms

expression :: TermParser Expression
expression =
  let group terms =
        case uncons terms of
          Just (t, []) -> t
          Just (t, ts) -> Call t ts

      toExpr t = case t of
        Parser.Int i -> Just $ Int i
        Parser.Variable i -> Just $ Variable i
        _ -> Nothing

      isSym s = case s of
        Parser.BinOp _ -> True
        _ -> False

      p = do
        left <- many1 $ satisfyMap toExpr
        bin <- optionMaybe (try $ satisfy isSym)
        case bin of
          Just (Parser.BinOp sym) -> BinOp sym (group left) <$> expression
          _ -> return (group left)
   in p

parse :: Parser.AST -> Either ParseError AST
parse = mapM statement
