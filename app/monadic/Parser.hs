module Parser ( module L
              , module G
              , module M
              , lexer
              , parser
              ) where

import qualified Data.ByteString as B

import qualified Parser.Grammar as GG 
import Parser.Grammar as G hiding (parser)
import qualified Parser.Lexer as LL
import Parser.Lexer as L hiding (lexer)
import Parser.Monad as M
import Parser.Token as T

lexer :: B.ByteString -> Either ParseError [T.Token]
lexer bs = M.runParser bs LL.lexer

parser :: B.ByteString -> Either ParseError T.Term
parser bs = M.runParser bs $ do
  toks <- LL.lexer
  GG.parser toks
