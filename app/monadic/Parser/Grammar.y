{
{-# LANGUAGE OverloadedStrings #-}
module Parser.Grammar where

import qualified Data.Text as T
import qualified Parser.Monad as P
import Parser.Spans
import Parser.Token
import qualified Error as E
}

%name parser expr
%tokentype { Token }
%monad { P.Parser }
%error { failure }

%token

ident  { Identifier $$ }
lambda { TokSymbol Lambda $$ }
'.'    { TokSymbol Dot $$ }
'('    { TokSymbol OpenParen $$ }
')'    { TokSymbol CloseParen $$ }

%%

expr
  : lambda ident '.' ap { Abs ($1 <> locate $4) (unlocate $2) $4 }
  | ap                  { $1 }

ap
  : ap atom { Ap (locate $1 <> locate $2) $1 $2 }
  | atom { $1 }

atom
  : '(' expr  ')' { $2 }
  | ident { Var (locate $1) (unlocate $1) }

{
failure :: [Token] -> P.Parser a
failure [] = P.parseError P.EmptyTokenStream
failure (tok:_) = do
  sp <- P.location
  P.parseError $ P.UnexpectedToken (Loc sp tok)
}
