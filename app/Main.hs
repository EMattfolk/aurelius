{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Compiler (compile)
import Parser (parse)
import Std
import Text.Parsec (runParser)

main :: IO ()
main =
  let ast = runParser parse () "Parsing program" "hello123 = 22"
      lua = compile <$> ast
   in do
        print ast
        print lua
