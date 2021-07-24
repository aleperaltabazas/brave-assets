{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Unit where

import Data.Text (Text, intercalate)

data Unit
  = Unit
  { name :: Text
  , rarity :: Text
  , arts :: [Text]
  } deriving (Show, Eq)

toCsvLine :: Unit -> Text
toCsvLine Unit {..} = intercalate ";" [name, rarity, "[" <> intercalate "," arts <> "]" <> "\n"]
