{
module Lexer where

import Data.Bits ((.&.), shiftR)
import Data.Char (ord)
import Data.Word (Word8)
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

utf8Encode' :: Char -> (Word8, [Word8])
utf8Encode' c =
  let (x, xs) = go (ord c)
  in (fromIntegral x, map fromIntegral xs)
 where
  go oc
   | oc <= 0x7f       = ( oc, [])
   | oc <= 0x7ff      = ( 0xc0 + (oc `shiftR` 6), [0x80 + oc .&. 0x3f])
   | oc <= 0xffff     = ( 0xe0 + (oc `shiftR` 12)
                        , [0x80 + ((oc `shiftR` 6) .&. 0x3f), 0x80 + oc .&. 0x3f])
   | otherwise        = ( 0xf0 + (oc `shiftR` 18)
                        , [0x80 + ((oc `shiftR` 12) .&. 0x3f)
                          , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                          , 0x80 + oc .&. 0x3f
                          ])

type AlexInput = (Char,[Word8],String)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (c,_,_) = c

lexer :: String -> [Token]
lexer str = go ('\n',[],str)
  where go inp__@(_,_bs,s) =
          case alexScan inp__ 0 of
                AlexEOF -> []
                AlexError _ -> error "lexical error"
                AlexSkip  inp__' _ln     -> go inp__'
                AlexToken inp__' len act -> act (take len s) : go inp__'

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (c,(b:bs),s) = Just (b,(c,bs,s))
alexGetByte (_,[],[])    = Nothing
alexGetByte (_,[],(c:s)) = case utf8Encode' c of
                             (b, bs) -> Just (b, (c, bs, s))
}
