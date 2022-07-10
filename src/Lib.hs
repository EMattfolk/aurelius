{-# LANGUAGE NoImplicitPrelude #-}

module Lib (someFunc) where

import Std
import Text.Parsec (alphaNum, lower, many, runParser, spaces)
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
  = Definition Identifier [Identifier]
  deriving (Show)

parseIdentifier :: Parser Identifier
parseIdentifier =
  do
    firstLetter <- lower
    rest <- many alphaNum
    return $ Identifier (firstLetter : rest)

parseStatement :: Parser Statement
parseStatement =
  do
    name <- parseIdentifier
    args <- many (spaces *> parseIdentifier)
    return $ Definition name args

parse :: Parser AST
parse = many parseStatement

someFunc :: IO ()
someFunc =
  let res = runParser parse () "Parsing variable" "hello123 arg1 arg2"
   in print res
