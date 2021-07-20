{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Wiki.BraveFrontier where

import Data.String.Interpolate (i)
import Data.Text (Text)
import Text.HTML.Scalpel

scrapeUnitImage :: Text -> Text -> IO (Maybe String)
scrapeUnitImage unitName id = scrapeURL [i|https://bravefrontierglobal.fandom.com/wiki/#{unitName}|] unitImage
 where
  unitImage :: Scraper String String
  unitImage = attr "src" $ "img" @: ["alt" @= [i|Unit ills full #{id}.png|]]
