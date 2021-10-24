{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$alphanum = [a-zA-Z09]

tokens :-

-- Whitespace insensitive
$white+                       ;

-- Comments
"#".*                         ;

-- Syntax

(λ|\\)                        { \_ -> Lambda }
\.                            { \_ -> Dot }
\(                            { \_ -> OpenParen }
\)                            { \_ -> CloseParen }
$alpha [$alpha $digit \_ \-]* { \s -> Identifier s }


{
data Token
  = Identifier String
  | Lambda
  | Dot
  | OpenParen
  | CloseParen
  deriving Show

lexer :: String -> [Token]
lexer = alexScanTokens
}
