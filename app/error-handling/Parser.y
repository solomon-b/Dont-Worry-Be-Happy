{
module Parser where

import qualified Lexer as L
import qualified Error as E
}

%name parser expr
%tokentype { L.TokenExt }
%error { parseError }
%monad { Either ParseError }

%token

ident  { L.TokenExt (L.Identifier $$) _ }
lambda { L.TokenExt L.Lambda _ }
'.'    { L.TokenExt L.Dot _ }
'('    { L.TokenExt L.OpenParen _ }
')'    { L.TokenExt L.CloseParen _ }

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
data ParseError
  = EmptyTokenStream
  | UnexpectedToken L.TokenExt
  deriving Show

instance E.RenderError ParseError where
  render EmptyTokenStream = E.RenderedError "ParseError: Empty token stream."
  render (UnexpectedToken (L.TokenExt tok pos)) =
    E.RenderedError $ "ParseError: Unexpected token '" <> L.serialize tok <> "' at line " <> show (L.line pos) <> ", column " <> show (L.col pos) <> "."

parseError :: [L.TokenExt] -> Either ParseError a
parseError [] = Left EmptyTokenStream
parseError (tok:_) = Left $ UnexpectedToken tok

data Term =
    Var String
  | Abs String Term
  | Ap Term Term
  deriving Show
}
