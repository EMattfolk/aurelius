{-# LANGUAGE NoImplicitPrelude #-}

module Parser
  ( parse,
    AST,
    Statement (..),
    Identifier (..),
    Symbol (..),
    Term (..),
  )
where

import Data.Char (isSpace)
import Std
import Text.Parsec (alphaNum, digit, endOfLine, eof, lookAhead, lower, many, many1, oneOf, optionMaybe, optional, runParser, satisfy, sepBy1, sepEndBy1, space, spaces, unexpected, (<?>))
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

newtype Symbol = Symbol String
  deriving (Show)

type AST = [Statement]

data Statement
  = Definition Identifier [Identifier] [Term]
  deriving (Show)

data Term
  = Int Integer
  | Variable Identifier
  | BinOp Symbol
  deriving (Show)

ws :: Parser ()
ws = void $ many (satisfy (\c -> isSpace c && c /= '\n'))

ws1 :: Parser ()
ws1 = void $ many1 (satisfy (\c -> isSpace c && c /= '\n'))

nl :: Parser ()
nl = ws *> (void endOfLine <|> eof)

muchWS :: Parser ()
muchWS = spaces *> optional eof

symbol :: Parser Symbol
symbol =
  Symbol <$> many1 (oneOf "|&<=>*/+-#$?!^~")

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
          v <- ws *> char '=' *> terms
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

term :: Parser Term
term =
  (Int <$> try int)
    <|> (Variable <$> try identifier)
    <|> (BinOp <$> try symbol)

terms :: Parser [Term]
terms =
  ws *> sepEndBy1 term ws <* nl <?> "expression"

parse :: Parser AST
parse = many statement <* eof
