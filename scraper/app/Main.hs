{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}

module Main where

import Control.Concurrent.Async
import Control.Monad (unless, forM, forM_, void, when)
import Data.Aeson
import Data.Either
import Data.List.Split (chunksOf)
import Data.Maybe
import Data.String.Interpolate (i)
import Data.Text.Conversions (FromText(fromText))
import qualified Network.Wiki.BraveFrontier as Wiki
import Options.Applicative

newtype ProgramOptions
  = ProgramOptions
  { output :: String
  } deriving (Show, Eq)

optionsParser :: Parser ProgramOptions
optionsParser = do
  output <- fromMaybe "units.json" <$> outputParser
  return ProgramOptions { .. }
 where
  outputParser =
    optional $ strOption (long "output" <> short 'o' <> metavar "PATH" <> help "File where to dump units. Default is units.json")

series = Nothing : map (Just . show) ([100, 200 .. 1900] ++ [7000] ++ [8000, 8100, 8600]) ++ [Just "Other"]

main :: IO ()
main = do
  ProgramOptions {..} <- execParser $ info (optionsParser <**> helper) (fullDesc <> header "brave-assets")
  putStrLn "Scraping unit lists..."
  lists             <- concat . catMaybes <$> forConcurrently series Wiki.scrapeUnitList
  (failures, units) <- eithers . concat <$> forM (chunksOf 5 lists) (mapConcurrently doScrape)
  putStrLn [i|Writing units to #{output}|]
  encodeFile output units
  putStrLn [i|Failed to scrape the following units:|]
  forM_ failures $ \u -> putStrLn [i|  #{u}|]
  undefined

 where
  doScrape u = do
    putStrLn [i|Scraping unit #{u}|]
    scraped <- Wiki.scrapeUnit u
    case scraped of
      Nothing -> do
        putStrLn [i|Failed to scrape #{u}|]
        return $ Left u
      Just unit -> return $ Right unit

eithers xs = (lefts xs, rights xs)
