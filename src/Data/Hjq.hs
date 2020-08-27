module Data.Hjq where
import Data.Text as T
import Data.Hjq.Parser

parseJqFilter :: String -> Either String JqFilter
parseJqFilter _ = Right JqNil