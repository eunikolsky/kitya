#!/usr/bin/env stack
-- stack script --resolver=lts-22.18 --package=tagsoup --package=text

{-# OPTIONS_GHC -Wall -Werror=missing-methods -Werror=missing-fields -Wprepositive-qualified-module #-}
{-# LANGUAGE DerivingStrategies, OverloadedStrings #-}

import Data.Text (Text)
import Data.Text qualified as T (strip)
import Data.Text.IO qualified as T (readFile)
import System.Environment (getArgs)
import Text.HTML.TagSoup (Tag(..), innerText, parseTags)

-- | Information about a Kitya's track on the map.
newtype Map = Map
  { title :: Text
  }
  deriving newtype (Show)

parseMapInfo :: FilePath -> IO Map
parseMapInfo f = do
  t <- T.readFile f
  let tags = parseTags t
      title = T.strip . innerText . take 2 . dropWhile (/= TagOpen "title" []) $ tags
  pure Map{title}

main :: IO ()
main = do
  srcFile <- head <$> getArgs
  print =<< parseMapInfo srcFile
