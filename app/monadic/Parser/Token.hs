{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Parser.Token where

import qualified Data.Text as T

import Parser.Spans

data Symbol =
    Lambda
  | Dot
  | OpenParen
  | CloseParen
  deriving Show

data Token =
    TokSymbol Symbol Span
  | Identifier (Loc T.Text)
  | EOF
  deriving Show

serialize :: Token -> T.Text
serialize = \case
  Identifier str -> unlocate str
  TokSymbol Lambda _ -> "\\"
  TokSymbol Dot _ -> "."
  TokSymbol OpenParen _ -> "("
  TokSymbol CloseParen _ -> ")"
  EOF -> ""

data Term =
    Var Span T.Text
  | Abs Span T.Text Term
  | Ap Span Term Term
  deriving Show

instance Located Term where
  locate = \case
    Var sp _ -> sp
    Abs sp _ _ -> sp
    Ap sp _ _ -> sp
