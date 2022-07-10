{-# LANGUAGE NoImplicitPrelude #-}

module Parser
  ( parse,
    AST,
    Statement (..),
    Identifier (..),
    Value (..),
  )
where

import Std
import Text.Parsec (alphaNum, digit, endOfLine, eof, lower, many, runParser, space, spaces)
import Text.Parsec.Char (char)
import Text.Parsec.Combinator (manyTill)
import Text.Parsec.Prim (try)
import Text.Parsec.String (Parser)

-- AST = statement...
-- statement = definition | ...
-- definition = identifier identifier... '=' value
-- value = int | call
-- call = identifier identifier...
-- identifier = [a-z][a-zA-Z0-9]+
-- int = [1-9][0-9]+

newtype Identifier = Identifier String
  deriving (Show)

type AST = [Statement]

data Statement
  = Definition Identifier [Identifier] Value
  deriving (Show)

data Value
  = Int Integer
  deriving (Show)

ws :: Parser ()
ws = spaces

eolws :: Parser ()
eolws = void $ manyTill space (try (void endOfLine <|> eof))

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
          ws -- TODO: Probably should not be here
          name <- identifier
          args <- many (try $ ws *> identifier)
          _ <- ws *> char '=' *> ws
          v <- value <* eolws
          return $ Definition name args v
   in definition

int :: Parser Integer
int =
  -- SAFETY: The parsing is correct, so the read will always succeed
  read <$> do
    first <- digit
    rest <- many digit
    return (first : rest)

value :: Parser Value
value = Int <$> int

parse :: Parser AST
parse = many statement
