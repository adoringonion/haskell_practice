data JqFilter
    = JqField Text JqFilter
    | JqIndex Int JqFilter
    | JqNil
    deriving (Show, Read, Eq)

parseJqFilter :: Text -> Either Text JqFilter
parseJqFilter s = undefined