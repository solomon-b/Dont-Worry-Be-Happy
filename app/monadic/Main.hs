module Main where

import Control.Monad (forever)
import qualified Data.ByteString as B
import qualified Parser as P
import qualified Error as E

main :: IO ()
main = forever $ do
  input <- B.getLine
  case P.parser input of
    Left err -> print $ E.render err
    Right ast -> print ast
