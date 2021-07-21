{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}

module Main where

import Control.Concurrent.Async
import Control.Exception
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
  dumpPathParser =
    optional $ strOption (long "dump-path" <> short 'd' <> metavar "PATH" <> help "Path where to dump images. Default is assets/")
  refreshParser = switch (long "refresh" <> short 'r' <> help "Redownload all images. Deactivated by default")

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
      e <- trySome $ do
        imgUrl <- Wiki.scrapeUnitImage (Deathmax.name unit) id
        case imgUrl of
          Nothing  -> putStrLn [i|No image for unit #{Deathmax.name unit}|]
          Just url -> do
            putStrLn [i|Downloading image for #{Deathmax.name unit}|]
            let latest = reverse . dropWhile (/= '/') . tail . dropWhile (/= '/') . reverse $ url
            void $ readCreateProcessWithExitCode (shell [i|curl #{latest} -o #{fileName unit}|]) ""
      case e of
        Left err -> do
          putStrLn [i|Failed to download #{Deathmax.name unit}|]
          print err
        Right _ -> return ()

 where
  trySome :: IO a -> IO (Either SomeException a)
  trySome = try

fileName :: Deathmax.Unit -> String
fileName =
  ("assets/" ++) . (++ ".png") . replace '\'' "" . replace '&' "and" . map toLower . replace ' ' "-" . fromText . Deathmax.name

replace :: Char -> String -> String -> String
replace look rep = concatMap (\c -> if c == look then rep else [c])
