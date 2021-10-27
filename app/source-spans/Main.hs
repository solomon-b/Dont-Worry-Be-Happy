module Main where

import Control.Monad (forever)
import Lexer
import Parser

main :: IO ()
main = forever $ do
  str <- getLine
  let tokens = lexer str
  print $ parser tokens
