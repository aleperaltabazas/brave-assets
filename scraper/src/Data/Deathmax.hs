{-# LANGUAGE DeriveGeneric #-}

module Data.Deathmax where

import Data.Aeson (FromJSON)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

type UnitMap = Map Text Unit

newtype Unit
  = Unit
  { name :: Text
  } deriving(Show, Read, Eq, Generic)

instance FromJSON Unit
