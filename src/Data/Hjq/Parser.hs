module Data.Hjq.Parser where
import Data.Text as T

data JqFilter
    = JqField Text JqFilter
    | JqIndex Int JqFilter
    | JqNil
    deriving (Show, Read, Eq)

parseJqFilter :: Text -> Either Text JqFilter
parseJqFilter s = undefined