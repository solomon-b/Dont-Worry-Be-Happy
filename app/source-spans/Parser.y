{
module Parser where

import qualified Lexer as L
import Span
}

%name parser expr
%tokentype { L.TokenExt }
%error { parseError }

%token

ident  { L.TokenExt (L.Identifier _) _ }
lambda { L.TokenExt L.Lambda _}
'.'    { L.TokenExt L.Dot _ }
'('    { L.TokenExt L.OpenParen _ }
')'    { L.TokenExt L.CloseParen _ }

%%

expr
  : lambda ident '.' expr { mkAbs $2 $4 }
  | ap { $1 }

ap
  : ap atom { mkApp $1 $2 }
  | atom { $1 }

atom
  : '(' expr  ')' { $2 }
  | ident { mkVar $1 }

{
mkAbs :: L.TokenExt -> Term -> Term
mkAbs ident@(L.TokenExt _ start) expr =
  let span = setStart (L.incCol (-1) start) $ getSpan expr
  in Abs span ((\(L.TokenExt (L.Identifier xs) sp) -> xs) ident) expr

mkApp :: Term -> Term -> Term
mkApp t1 t2= Ap (getSpan t1 <> getSpan t2) t1 t2

mkVar :: L.TokenExt -> Term
mkVar (L.TokenExt (L.Identifier xs) start) = Var (Span start (L.incCol (length xs - 1) start)) xs

parseError :: [L.TokenExt] -> a
parseError [] = error  "ParseError: Empty token stream."
parseError ((L.TokenExt tok pos):_) =
  error $ "ParseError: Unexpected token '" <> show tok <> "' at line " <> show (L.line pos) <> ", column " <> show (L.col pos) <> "."

getSpan :: Term -> Span
getSpan (Var span _) = span
getSpan (Abs span _ _) = span
getSpan (Ap span _ _) = span

data Term =
    Var Span String
  | Abs Span String Term
  | Ap  Span Term   Term
  deriving Show
}
