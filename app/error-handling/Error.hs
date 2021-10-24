module Error where

class RenderError e where
  render :: e -> RenderedError

newtype RenderedError = RenderedError { message :: String }
  deriving Show
