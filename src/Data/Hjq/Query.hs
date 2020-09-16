{-# LANGUAGE OverloadedStrings #-}
module Data.Hjq.Query where
import Control.Lens
import Control.Monad
import Data.Monoid
import Data.Aeson
import Data.Aeson.Lens
import Data.Hjq.Parser
import qualified Data.Vector as V
import Data.Text as T

applyFilter :: JqFilter -> Value -> Either T.Text Value
applyFilter (JqField fieldName n) obj@(Object _)
  = join $ noteNotFoundError fieldName (fmap (applyFilter n) (obj ^? key fieldName))
applyFilter (JqIndex index n) obj@(Object _)
  = join $ noteOutOfRangeError index (fmap (applyFilter n) (obj ^? nth index))
applyFilter JqNil v = Right v
applyFilter f o = Left $ "unexpected pattern : " <> tshow f <> " : " <> tshow o

noteNotFoundError :: T.Text -> Maybe a -> Either T.Text a
noteNotFoundError _ (Just x) = Right x
noteNotFoundError s Nothing = Left $ "field name not found " <> s

noteOutOfRangeError :: Int -> Maybe a -> Either T.Text a
noteOutOfRangeError _ (Just x) = Right x
noteOutOfRangeError s Nothing = Left $ "out of range : " <> tshow s

tshow :: Show a => a -> T.Text
tshow = T.pack . show