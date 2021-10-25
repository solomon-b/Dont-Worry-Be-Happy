{
module Parser where

import qualified Lexer as L
}

%name parser expr
%tokentype { L.Token }
%error { parseError }

%token

'+' { L.Plus }
'-' { L.Negate }
'*' { L.Multiply }
'(' { L.OpenParen }
')' { L.CloseParen }
int { L.Num $$ }

%left '+' '*' '-'

%%

expr
  : expr '+' expr1 { Add $1 $3 }
  | expr1 { $1 }

expr1
  : expr1 '*' expr2 { Multiply $1  $3 }
  | expr2 { $1 }

expr2
  : int { Num $1 }
  | '-' expr2 { Negate $2 }
  | '(' expr ')' { $2 }


{
parseError :: [L.Token] -> a
parseError [] = error "ParseError: Empty token stream."
parseError (tok:_) = error $ "ParseError: Unexpected token '" <> show tok <> "'."

data Expr =
    Num Int
  | Add Expr Expr
  | Negate Expr
  | Multiply Expr Expr
  deriving Show
}
