{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Parser.Lexer where

import Data.Bits ((.&.), shiftR)
import Data.Char (ord)
import qualified Data.Text as T
import Data.Word (Word8)

import Parser.Monad
import Parser.Spans
import Parser.Token
import qualified Error as E
}

$digit = 0-9
$alpha = [a-zA-Z]
$alphanum = [a-zA-Z09]

tokens :-

-- Whitespace insensitive
$white+                       ;

-- Comments
"#".*                         ;

-- Syntax

(λ|\\)                        { symbol Lambda }
\.                            { symbol Dot }
\(                            { symbol OpenParen }
\)                            { symbol CloseParen }
$alpha [$alpha $digit \_ \-]* { token Identifier }

{
scan :: Parser Token
scan = do
  input <- getInput
  case alexScan input 0 of
    AlexEOF -> pure EOF
    AlexError (AlexInput pos _ _ _) ->
      parseError $ InvalidLexeme pos
    AlexSkip rest len -> do
      advance rest
      scan
    AlexToken rest nbytes action -> do
      advance rest
      action (slice nbytes input)

lexer :: Parser [Token]
lexer = do
  tok <- scan
  case tok of
    EOF -> pure []
    x -> (x :) <$> lexer
}
