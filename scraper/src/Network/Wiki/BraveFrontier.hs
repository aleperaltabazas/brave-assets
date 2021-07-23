{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Wiki.BraveFrontier where

import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Unit
import Text.HTML.Scalpel

page :: String
page = "https://bravefrontierglobal.fandom.com/wiki"

scrapeUnitImage :: Text -> Text -> IO (Maybe String)
scrapeUnitImage unitName id = scrapeURL [i|#{page}/#{unitName}|] unitImage
 where
  unitImage :: Scraper String String
  unitImage = attr "src" $ "img" @: ["alt" @= [i|Unit ills full #{id}.png|]]

scrapeUnitList :: Maybe String -> IO (Maybe [String])
scrapeUnitList list = scrapeURL [i|#{page}/Unit_List#{maybe "" (":" ++) list}|] unitList
 where
  unitList = tail <$> chroot ("table" @: [hasClass "wikitable"]) (chroot "tbody" $ chroots "tr" unitName)

  unitName = chroot "td" (attr "title" "a")

scrapeUnit :: String -> IO (Maybe Unit)
scrapeUnit name = scrapeURL [i|#{page}/#{name}|] unit
 where
  unit :: Scraper String Unit
  unit = do
    rarity <- scrapeRarity
    arts   <- scrapeArts
    return Unit { .. }

  scrapeRarity = takeWhile isDigit . (!! 2) <$> chroot
    ("div" @: [hasClass "unit-info", hasClass "unit-box"])
    (chroot (("table" @: [hasClass "article-table", hasClass "tight"]) // "tbody") $ chroots "tr" $ chroot "td" (text "a"))
  scrapeArts = map trimUrl <$> chroot
    ("div" @: [hasClass "unit-gallery", hasClass "unit-box"])
    (  chroot ("div" @: [hasClass "tabber", hasClass "wds-tabber"])
    $  chroots ("div" @: [hasClass "wds-tab__content"])
    $  attr "src"
    $  "img"
    @: [match isFullArtImage]
    )

  isFullArtImage attr value = attr == "alt" && "Unit ills full" `isPrefixOf` value

  trimUrl = reverse . dropWhile (/= '/') . tail . dropWhile (/= '/') . reverse

