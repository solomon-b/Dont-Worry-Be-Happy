module Main where

import Lexer
import Parser

main :: IO ()
main = do
  str <- getLine
  let tokens = lexer str
  print $ parser tokens
