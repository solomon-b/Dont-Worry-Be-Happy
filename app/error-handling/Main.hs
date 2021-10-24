module Main where

import Control.Monad (forever)
import Data.Bifunctor (first)
import qualified Lexer as L
import qualified Parser as P
import qualified Error as E

main :: IO ()
main = forever $ do
  str <- getLine
  case L.lexer str of
    Left err -> print (E.render err)
    Right tokens -> print $ first E.render $ P.parser tokens
