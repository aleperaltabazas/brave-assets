{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad (unless, forM, forM_, void, when)
import Control.Monad.IO.Class
import GHC.Conc
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Internal as BS (c2w)
import Data.List.Split (chunksOf)
import Data.Maybe
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Data.Unit
import qualified Network.Wiki.BraveFrontier as Wiki
import Options.Applicative

newtype ProgramOptions
  = ProgramOptions
  { output :: FilePath
  } deriving (Show, Eq)

optionsParser :: Parser ProgramOptions
optionsParser = do
  output <- fromMaybe "units.csv" <$> outputParser
  return ProgramOptions { .. }
 where
  outputParser =
    optional $ strOption (long "output" <> short 'o' <> metavar "PATH" <> help "File where to dump units. Default is units.json")

series = Nothing : map (Just . show) ([100, 200 .. 1800] ++ [7000] ++ [8000, 8100 .. 8600]) ++ [Just "Other"]

main :: IO ()
main = do
  ProgramOptions {..} <- execParser $ info (optionsParser <**> helper) (fullDesc <> header "brave-assets")
  putStrLn "Scraping unit lists..."
  unitNames <- concat . catMaybes <$> mapConcurrently Wiki.scrapeUnitList series
  Text.writeFile output "name;rarity;arts\n"
  errorsTVar <- newTVarIO ([] :: [Text])
  forM_ unitNames $ doScrape errorsTVar output
  errors <- readTVarIO errorsTVar
  putStrLn "Failed to scrape the following units:"
  forM_ errors $ \err -> Text.putStrLn [i|  #{err}|]

 where
  doScrape errors output unitName = do
    Text.putStrLn [i|Scraping #{unitName}|]
    unit <- Wiki.scrapeUnit unitName
    case unit of
      Nothing -> atomically $ modifyTVar errors (++ [unitName])
      Just u  -> Text.appendFile output $ toCsvLine u
