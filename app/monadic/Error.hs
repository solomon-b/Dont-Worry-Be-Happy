module Error where

import qualified Data.Text as T

class RenderError e where
  render :: e -> RenderedError

newtype RenderedError = RenderedError { message :: T.Text }
  deriving Show
