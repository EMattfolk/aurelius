{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Parser (parse)
import Std
import Text.Parsec (runParser)

main :: IO ()
main =
  let res = runParser parse () "Parsing variable" "hello123 arg1 arg2 = 22"
   in print res
