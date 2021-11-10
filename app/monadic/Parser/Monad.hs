{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Parser.Monad where

import qualified Codec.Binary.UTF8.String as UTF8
import Control.Monad.Except
import Control.Monad.State
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.UTF8 as UTFBS
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import qualified Error as E
import Parser.Spans
import Parser.Token

data ParserState = ParserState
  { parseInput :: AlexInput
  , parseSpan :: Span
  }

initState :: B.ByteString -> ParserState
initState bs = ParserState
  { parseInput      = AlexInput (AlexSourcePos 0 1) '\n' bs []
  , parseSpan       = Span (AlexSourcePos 0 1) (AlexSourcePos 0 1)
  }
  
newtype Parser a = Parser { unParser :: StateT ParserState (Except ParseError) a }
    deriving newtype (Functor, Applicative, Monad, MonadState ParserState, MonadError ParseError)

runParser :: B.ByteString -> Parser a -> Either ParseError a
runParser bs lex = runExcept $ evalStateT (unParser lex) (initState bs)

------------------------
--- State Management ---
------------------------

{-# INLINE advance #-}
advance :: AlexInput -> Parser ()
advance input@AlexInput{ lexPos } = do
  modify' $ \s ->
    s { parseInput = input
      , parseSpan = Span (end $ parseSpan s) lexPos
      }

{-# INLINE setInput #-}
setInput :: AlexInput -> Parser ()
setInput input = modify $ \s -> s { parseInput = input }

{-# INLINE getInput #-}
getInput :: Parser AlexInput
getInput = gets parseInput

{-# INLINE getParseColumn #-}
getParseColumn :: Parser Int
getParseColumn = gets (col . lexPos . parseInput)

{-# INLINE location #-}
location :: Parser Span
location = gets parseSpan

{-# INLINE located #-}
located :: a -> Parser (Loc a)
located a = do
  sp <- location
  pure $ Loc sp a

----------------------
--- Error Handling ---
----------------------

data ParseError =
    EmptyTokenStream
  | UnexpectedToken (Loc Token)
  | InvalidLexeme AlexSourcePos
  deriving Show

instance E.RenderError ParseError where
  render EmptyTokenStream = E.RenderedError "ParseError: Empty token stream."
  render (UnexpectedToken tok) =
    let tok' = serialize $ unlocate tok
        span = start $ locate tok
        line' = T.pack $ show $ line span
        col' = T.pack $ show $ col span
    in E.RenderedError $ "ParseError: Unexpected token '" <> tok' <> "' at line " <> line' <> ", column " <> col' <> "."
  render (InvalidLexeme pos) = E.RenderedError $ "LexError: Invalid Lexeme at line " <> T.pack (show $ line pos) <> ", column " <> T.pack (show $ col pos) <> "."

parseError :: ParseError -> Parser a
parseError err = throwError err

--------------
--- Tokens ---
--------------

{-# INLINE token #-}
token :: (Loc T.Text -> Token) -> B.ByteString -> Parser Token
token k bs = k <$> located (TE.decodeUtf8 bs)

{-# INLINE symbol #-}
symbol :: Symbol -> B.ByteString -> Parser Token
symbol sym _ = TokSymbol sym <$> location

-----------------------
--- Alex Primitives ---
-----------------------

data AlexInput = AlexInput
  { lexPos :: AlexSourcePos
  , lexPrevChar   :: Char
  , lexBytes :: B.ByteString
  -- ^ current input bytestring
  , lexCharBytes :: [Word8]
  -- ^ remaining bytes in current character
  }

{-# INLINE nextLine #-}
nextLine :: B.ByteString -> AlexInput -> AlexInput
nextLine rest AlexInput{..} = AlexInput
  { lexPos = lexPos { line = line lexPos + 1, col = 1 }
  , lexPrevChar = '\n'
  , lexBytes = rest
  , lexCharBytes = []
  }

{-# INLINE nextCol #-}
nextCol :: Char -> B.ByteString -> AlexInput -> AlexInput
nextCol c rest AlexInput{..} = AlexInput
  { lexPos = lexPos { col = col lexPos + 1 }
  , lexPrevChar = c
  , lexBytes = rest
  , ..
  }

{-# INLINE popBufferedBytes #-}
popBufferedBytes :: AlexInput -> Maybe (Word8, AlexInput)
popBufferedBytes AlexInput{..} = 
  case lexCharBytes of
    [] -> Nothing
    (b : bs) -> Just (b, AlexInput { lexCharBytes = bs, .. })

{-# INLINE bufferBytes #-}
bufferBytes :: Char -> [Word8] -> B.ByteString -> AlexInput -> AlexInput
bufferBytes c bytes rest AlexInput{..} = AlexInput
  { lexPrevChar = c
  , lexBytes = rest
  , lexCharBytes = bytes
  , ..
  }

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte input@AlexInput{..} =
  case popBufferedBytes input of
    Nothing -> advance <$> UTFBS.uncons lexBytes
    ok      -> ok
  where
    advance :: (Char, B.ByteString) -> (Word8, AlexInput)
    advance ('\n', rest) = (B.c2w '\n', nextLine rest input)
    advance (c, rest)   =
      case UTF8.encodeChar c of
        [b]    -> (b, nextCol c rest input)
        (b:bs) -> (b, bufferBytes c bs rest input)
        []     -> error "The impossible happened! A Char decoded to 0 bytes."

alexPrevInputChar :: AlexInput -> Char
alexPrevInputChar = lexPrevChar

{-# INLINE slice #-}
slice :: Int -> AlexInput -> B.ByteString
slice n AlexInput{..} = B.take n lexBytes
