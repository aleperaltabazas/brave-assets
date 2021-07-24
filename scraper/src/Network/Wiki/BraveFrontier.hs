{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Wiki.BraveFrontier where


import Data.Char
import Data.String.Interpolate (i)
import Data.Text (Text, replace)
import qualified Data.Text as Text
import Data.Unit (Unit(..))
import Text.HTML.Scalpel

page :: String
page = "https://bravefrontierglobal.fandom.com/wiki"

star :: Text
star = "★"

escapedStar :: Text
escapedStar = "\226\152\133"

scrapeUnitList :: Maybe String -> IO (Maybe [Text])
scrapeUnitList list = scrapeURL [i|#{page}/Unit_List#{maybe "" (":" ++) list}|] unitList

 where
  unitList = tail <$> chroot ("table" @: [hasClass "wikitable"]) (chroot "tbody" $ chroots "tr" unitName)

  unitName = do
    s <- chroot "td" (attr "title" "a")
    return $ replace escapedStar star s

scrapeUnit :: Text -> IO (Maybe Unit)
scrapeUnit name = do
  s <- scrapeURL [i|#{page}/#{name}|] unit
  return $! s
 where
  unit :: Scraper Text Unit
  unit = do
    rarity <- scrapeRarity
    arts   <- scrapeArts
    return Unit { .. }

  scrapeRarity = rectifyRarity . (!! 2) <$> chroot
    ("div" @: [hasClass "unit-info", hasClass "unit-box"])
    (chroot (("table" @: [hasClass "article-table", hasClass "tight"]) // "tbody") $ chroots "tr" $ chroot "td" (text "a"))

  scrapeArts = map trimUrl <$> chroot
    ("div" @: [hasClass "unit-gallery", hasClass "unit-box"])
    ( chroot ("div" @: [hasClass "tabber", hasClass "wds-tabber"])
    $ chroots ("div" @: [hasClass "wds-tab__content"])
    $ chroot "center"
    $ attr "href" "a"
    )

  trimUrl = Text.reverse . Text.dropWhile (/= '/') . Text.tail . Text.dropWhile (/= '/') . Text.reverse

  rectifyRarity t
    | Text.null t           = "Omni"
    | isDigit $ Text.head t = Text.take 1 t <> star
    | otherwise             = t
