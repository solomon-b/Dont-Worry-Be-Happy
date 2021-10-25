{
module Lexer where
}

%wrapper "basic"

$digit = 0-9

tokens :-

-- Whitespace insensitive
$white+                       ;

-- Syntax

\+                            { \_ -> Plus }
\-                            { \_ -> Negate }
\*                            { \_ -> Multiply }
\/                            { \_ -> Divide }
\(                            { \_ -> OpenParen }
\)                            { \_ -> CloseParen }
$digit+                       { \s -> Num (read s) }


{
data Token
  = Num Int
  | Plus
  | Negate
  | Multiply
  | Divide
  | OpenParen
  | CloseParen
  deriving Show

lexer :: String -> [Token]
lexer = alexScanTokens
}
