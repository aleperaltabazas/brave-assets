{-# LANGUAGE DeriveGeneric #-}

module Data.Unit where

import Data.Aeson
import Data.Text
import GHC.Generics

data Unit
  = Unit
  { name :: String
  , rarity :: String
  , arts :: [String]
  } deriving (Show, Eq, Generic)

instance ToJSON Unit
