{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Compiler (compile)
import qualified Parser
import Parser2 (parse)
import Std
import Text.Parsec (runParser)

main :: IO ()
main =
  do
    prog <- readFile "program.au"
    let ast = parse =<< runParser Parser.parse () "Parsing program" prog
    let lua = compile <$> ast
    print (runParser Parser.parse () "Parsing program" prog)
    print lua
    case lua of
      Right l -> writeFile "program.lua" l
      _ -> return ()
