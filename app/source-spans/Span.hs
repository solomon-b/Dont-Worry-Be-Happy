module Span where

import qualified Lexer as L

data Span = Span { start :: L.AlexSourcePos, end :: L.AlexSourcePos }
  deriving Show

instance Semigroup Span where
  (Span start _) <> (Span _ end) = Span start end

overStart :: (L.AlexSourcePos -> L.AlexSourcePos) -> Span -> Span
overStart f (Span start end) = Span (f start) end

overEnd :: (L.AlexSourcePos -> L.AlexSourcePos) -> Span -> Span
overEnd f (Span start end) = Span start (f end)

setStart :: L.AlexSourcePos -> Span -> Span
setStart sp (Span _ end) = Span sp end

setEnd :: L.AlexSourcePos -> Span -> Span
setEnd sp (Span start _) = Span start sp
