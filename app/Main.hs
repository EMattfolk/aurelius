{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Compiler (compile)
import Parser (parse)
import Std
import Text.Parsec (runParser)

main :: IO ()
main =
  do
    prog <- readFile "program.au"
    let ast = runParser parse () "Parsing program" prog
    let lua = compile <$> ast
    print ast
    print lua
    case lua of
      Right l -> writeFile "program.lua" l
      _ -> return ()
