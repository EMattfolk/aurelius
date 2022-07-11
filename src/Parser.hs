{-# LANGUAGE NoImplicitPrelude #-}

module Parser
  ( parse,
    AST,
    Statement (..),
    Identifier (..),
    Expression (..),
  )
where

import Data.Char (isSpace)
import Std
import Text.Parsec (alphaNum, digit, endOfLine, eof, lookAhead, lower, many, many1, optional, runParser, satisfy, space, spaces, unexpected, (<?>))
import Text.Parsec.Char (char)
import Text.Parsec.Combinator (manyTill)
import Text.Parsec.Prim (try)
import Text.Parsec.String (Parser)

-- AST = statement...
-- statement = definition | ...
-- definition = identifier identifier... '=' expression
-- expression = int | call
-- call = identifier identifier...
-- identifier = [a-z][a-zA-Z0-9]+
-- int = [1-9][0-9]+

newtype Identifier = Identifier String
  deriving (Show)

type AST = [Statement]

data Statement
  = Definition Identifier [Identifier] Expression
  deriving (Show)

data Expression
  = Int Integer
  | Variable Identifier
  | Call Expression [Expression]
  deriving (Show)

ws :: Parser ()
ws = void $ many (satisfy (\c -> isSpace c && c /= '\n'))

nl :: Parser ()
nl = ws *> (void endOfLine <|> eof)

muchWS :: Parser ()
muchWS = spaces *> optional eof

identifier :: Parser Identifier
identifier =
  do
    firstLetter <- lower
    rest <- many alphaNum
    return $ Identifier (firstLetter : rest)

statement :: Parser Statement
statement =
  let definition =
        do
          muchWS
          name <- identifier
          args <- many (try $ ws *> identifier)
          v <- ws *> char '=' *> expression
          muchWS
          return $ Definition name args v
   in definition

int :: Parser Integer
int =
  -- SAFETY: The parsing is correct, so the read will always succeed
  read <$> do
    first <- digit
    rest <- many digit
    return (first : rest)

expression :: Parser Expression
expression =
  ( do
      atoms <- many1 (ws *> ((Int <$> try int) <|> (Variable <$> try identifier))) <* nl
      case atoms of
        [] -> unexpected "Empty atoms list though many1 was used."
        [x] -> return x
        x : xs -> return $ Call x xs
  )
    <?> "expression"

parse :: Parser AST
parse = many statement <* eof
