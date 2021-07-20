{-# LANGUAGE OverloadedStrings #-}

module Network.Github.Deathmax
  ( fetchUnits
  )
where

import Data.Deathmax
import Network.HTTP.Req

endpoint = https "raw.githubusercontent.com" /: "cheahjs" /: "bravefrontier_data" /: "master" /: "info.json"

fetchUnits :: IO UnitMap
fetchUnits = runReq defaultHttpConfig $ do
  res <- req GET endpoint NoReqBody jsonResponse mempty
  return $ responseBody res
