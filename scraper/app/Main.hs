{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}

module Main where

import Control.Concurrent.Async
import Control.Monad (unless, forM_, void, when)
import Data.Char (toLower)
import qualified Data.Deathmax as Deathmax
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe
import Data.String.Interpolate (i)
import Data.Text.Conversions (FromText(fromText))
import qualified Network.Github.Deathmax as Deathmax
import qualified Network.Wiki.BraveFrontier as Wiki
import Options.Applicative
import System.Directory (createDirectoryIfMissing, doesFileExist, removePathForcibly)
import System.Process (readCreateProcessWithExitCode, shell)

data ProgramOptions
  = ProgramOptions
  { dumpPath :: FilePath
  , refresh :: Bool
  } deriving (Show, Eq)

optionsParser :: Parser ProgramOptions
optionsParser = do
  dumpPath <- fromMaybe "assets" <$> dumpPathParser
  refresh  <- refreshParser
  return ProgramOptions { .. }
 where
  dumpPathParser = optional $ strOption (long "dump-path" <> short 'd' <> metavar "PATH" <> help "Path where to dump images")
  refreshParser  = switch (long "refresh" <> short 'r' <> help "Redownload all images")

main :: IO ()
main = do
  ProgramOptions {..} <- execParser $ info (optionsParser <**> helper) (fullDesc <> header "brave-assets")
  putStrLn "Fetching units from Deathmax..."
  units <- Deathmax.fetchUnits
  putStrLn "Finished fetching units. Scraping images..."
  when refresh $ removePathForcibly dumpPath
  createDirectoryIfMissing False "assets"
  forM_ (chunksOf 5 . Map.toList $ units) $ mapConcurrently_ $ \(id, unit) -> do
    exists <- doesFileExist (fileName unit)
    unless exists $ do
      putStrLn [i|Scraping #{Deathmax.name unit}|]
      imgUrl <- Wiki.scrapeUnitImage (Deathmax.name unit) id
      case imgUrl of
        Nothing  -> putStrLn [i|No image for unit #{Deathmax.name unit}|]
        Just url -> do
          let latest = reverse . dropWhile (/= '/') . tail . dropWhile (/= '/') . reverse $ url
          void $ readCreateProcessWithExitCode (shell [i|curl #{latest} -o assets/#{fileName unit}|]) ""
 where
  fileName :: Deathmax.Unit -> String
  fileName unit = (++ ".png") . map toLower . replace ' ' "-" . fromText . Deathmax.name $ unit

  replace :: Char -> String -> String -> String
  replace look rep = concatMap (\c -> if c == look then rep else [c])
