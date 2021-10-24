{
module Parser where

import qualified Lexer as L
}

%name parser expr
%tokentype { L.Token }
%error { parseError }

%token

ident  { L.Identifier $$ }
lambda { L.Lambda }
'.'    { L.Dot }
'('    { L.OpenParen }
')'    { L.CloseParen }

%%

expr
  : lambda ident '.' ap { Abs $2 $4 }
  | ap                  { $1 }

ap
  : ap atom { Ap $1 $2 }
  | atom { $1 }

atom
  : '(' expr  ')' { $2 }
  | ident { Var $1 }

{
parseError :: [L.Token] -> a
parseError [] = error "ParseError: Empty token stream."
parseError (tok:_) = error $ "ParseError: Unexpected token '" <> show tok <> "'."

data Term =
    Var String
  | Abs String Term
  | Ap Term Term
  deriving Show
}
