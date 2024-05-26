-- License: GNU GPL v3.0 (see `../license`)

{-# LANGUAGE DeriveGeneric, DerivingVia, ImportQualifiedPost, NamedFieldPuns, OverloadedStrings, StandaloneDeriving, TypeApplications #-}

import Control.Monad ((<=<), forM_)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict)
import Data.Aeson.Encode.Pretty (Config(..), Indent(..), defConfig, encodePretty')
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BSL (writeFile, toStrict)
import Data.Char (isDigit)
import Data.List (isInfixOf, isPrefixOf, find, partition, sortOn, stripPrefix)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE (nonEmpty)
import Data.Maybe (mapMaybe, fromJust, fromMaybe)
import Data.Ord (Down(..))
import Data.Text (Text)
import Data.Text qualified as T (strip, pack, isInfixOf, unpack, split, replace)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO qualified as T (readFile, writeFile)
import Data.Time (DiffTime, LocalTime, defaultTimeLocale)
import Data.Time.Clock (secondsToDiffTime)
import Data.Time.Format (formatTime, parseTimeM)
import GHC.Generics (Generic, Generically(..))
import Language.ECMAScript3 (Expression(..), Id(..), JavaScript, PrefixOp(..), Prop(..), SourcePos, Statement(..), VarDecl(..), parse, program, unJavaScript)
import Lens.Micro ((^?), (^..))
import Lens.Micro.Aeson (_Double, _Integer, _String, key, values)
import Prelude hiding (map)
import System.Directory (copyFile, createDirectoryIfMissing, listDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit (die)
import System.FilePath ((-<.>), (</>), dropExtension, takeBaseName, takeFileName, takeExtension)
import Text.Blaze.Html.Renderer.Pretty
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.HTML.TagSoup (Tag(..), innerText, parseTags)
import Text.HTML.TagSoup.Match (tagComment)
import Text.Printf (printf)
import Text.Read (readMaybe)

-- | WGS84 geographic coordinate.
-- `lng` instead of `long` to be compatible with leafletjs's `LatLng` constructor
-- https://leafletjs.com/reference.html#latlng
data Coord = Coord { lat :: !Double, lng :: !Double }
  deriving (Show, Generic)

deriving via Generically Coord instance ToJSON Coord
deriving via Generically Coord instance FromJSON Coord

newtype Distance = Distance { getDistanceMeters :: Double }
  deriving (Generic)

deriving via Generically Distance instance ToJSON Distance
deriving via Generically Distance instance FromJSON Distance

newtype Speed = Speed { getSpeedMetersPerSecond :: Double }
  deriving (Generic)

deriving via Generically Speed instance ToJSON Speed
deriving via Generically Speed instance FromJSON Speed

data Elevation = Elevation
  { elRange :: !(Distance, Distance)
  , elTotalAscent :: !Distance
  , elTotalDescent :: !Distance
  }
  deriving (Generic)

deriving via Generically Elevation instance ToJSON Elevation
deriving via Generically Elevation instance FromJSON Elevation

data ExtendedMap = ExtendedMap
  { emStartTime :: !LocalTime
  , emTimezone :: !Text
  , emDistance :: !Distance
  , emDuration :: !DiffTime
  , emElevation :: !Elevation
  , emAvgSpeed :: !Speed
  , emAvgMovingSpeed :: !Speed
  , emMaxSpeed :: !Speed
  }
  deriving (Generic)

deriving via Generically ExtendedMap instance ToJSON ExtendedMap
deriving via Generically ExtendedMap instance FromJSON ExtendedMap

-- | Information about a Kitya's track on the map.
data Map = Map
  { filename :: !FilePath
  -- ^ filename of the mirrored HTML file or the basename of garmin's json file
  , sourceMapFilename :: !FilePath
  -- ^ filename of the source text file with map data, extracted from the source
  -- URL, which is recorded by HTTrack
  , title :: !Text
  , start :: !Coord
  , finish :: !Coord
  -- these two fields should really be one, but I don't want to spend time
  -- testing the json encoder in that case
  , encodedPolylines :: !(Maybe (NonEmpty Text))
  , polyline :: !(Maybe [Coord])
  , extendedMap :: !(Maybe ExtendedMap)
  }
  deriving (Generic)

deriving via Generically Map instance ToJSON Map
deriving via Generically Map instance FromJSON Map

parseMapInfo :: FilePath -> IO Map
parseMapInfo f = do
  t <- T.readFile f
  let tags = parseTags t
      title = T.strip . innerText . take 2 . dropWhile (/= TagOpen "title" []) $ tags
      (start, finish, encodedPolylines) = extractTrack tags
      filename = takeFileName f
      sourceMapFilename = extractSourceMapFilename tags
  pure Map{filename, sourceMapFilename, title, start, finish, encodedPolylines = Just encodedPolylines, polyline = Nothing, extendedMap = Nothing}

getJSText :: [Tag Text] -> Text
getJSText = innerText . take 2 . dropWhile (/= TagOpen "script" [("type", "text/javascript")])

parseJS :: Text -> JavaScript SourcePos
parseJS = either (error . ("couldn't parse: " <>) . show) id . parse program ""

{- note: looking for the necessary things in the AST is a pain (I wonder if
 - there is something HXT-like to use arrows for tree processing?), so I
 - dumped the AST of one map file and used it to find where the interesting
 - things are; all the `index.{4}\.html` files have the same structure,
 - although with a varying number of `encodedPoints_*` definitions, so it's
 - fine to have a rigid parser; the files aren't going to change anyway
 -}
extractTrack :: [Tag Text] -> (Coord, Coord, NonEmpty Text)
extractTrack tags = (getCoord "s" def, getCoord "f" def, getPolylines def)
  where
    def = findTrackDefBlock . parseJS . getJSText $ tags
    findTrackDefBlock = concatMap (mapMaybe gBrowserIsCompatibleIf) . mapMaybe initializeFunc . unJavaScript
    initializeFunc (FunctionStmt _ (Id _ "initialize") _ ss) = Just ss
    initializeFunc _ = Nothing
    gBrowserIsCompatibleIf (IfSingleStmt _ (CallExpr _ (VarRef _ (Id _ "GBrowserIsCompatible")) []) (BlockStmt _ ss)) = Just ss
    gBrowserIsCompatibleIf _ = Nothing

    getCoord name = markerCoord . concat . concatMap (mapMaybe (coordVar name))
    coordVar name (VarDeclStmt _ [VarDecl _ (Id _ _name) (Just (NewExpr _ (VarRef _ (Id _ "GMarker")) [NewExpr _ (VarRef _ (Id _ "GLatLng")) ss, ObjectLit _ [(PropId _ (Id _ "title"), StringLit _ _)]]))]) | name == _name = Just ss
    coordVar _ _ = Nothing

    -- [NumLit _ 46.126683333333,PrefixExpr _ PrefixMinus (NumLit _ 121.51863333333)]
    markerCoord :: Show a => [Expression a] -> Coord
    markerCoord [latE, longE] = Coord{lat=evalExpr latE, lng=evalExpr longE}
    markerCoord u = error $ "unexpected coords " <> show u

    evalExpr (NumLit _ x) = x
    evalExpr (PrefixExpr _ PrefixMinus (NumLit _ x)) = -x
    evalExpr u = error $ "unexpected expression " <> show u

    getPolylines = fromJust . NE.nonEmpty . fmap T.pack . concatMap (mapMaybe encodedPointsVar)

    encodedPointsVar (VarDeclStmt _ [VarDecl _ (Id _ name) (Just (StringLit _ t))])
      | "encodedPoints_" `isPrefixOf` name = Just t
    encodedPointsVar _ = Nothing

extractSourceMapFilename :: [Tag Text] -> FilePath
extractSourceMapFilename = getFilename . fromJust . find (tagComment ("Mirrored from " `T.isInfixOf`))
  where
    -- comment = " Mirrored from lj.karlson.ru/map/index.php?map=SmithRock.txt by HTTrack Website Copier/3.x [XR&CO'2014], Mon, 11 Oct 2021 05:57:27 GMT "
    getFilename (TagComment comment) = T.unpack . (!! 4) . T.split (\c -> c == ' ' || c == '=') $ comment
    getFilename x = error $ "impossible tag: " <> show x

listMapFiles :: FilePath -> IO [FilePath]
listMapFiles dir = fmap (dir </>) . filter isMapFile <$> listDirectory dir
  where
    -- index0206.html
    isMapFile f = all ($ f)
      [ ("index" `isPrefixOf`)
      , (== ".html") . takeExtension
      , (== 14) . length
      ]

encode :: ToJSON a => a -> ByteString
encode = encodePretty' conf
  where conf = defConfig { confIndent = Spaces 2, confTrailingNewline = True }

removeNewlines :: Map -> Map
removeNewlines map@Map{encodedPolylines} =
  map { encodedPolylines = fmap removeNewline <$> encodedPolylines }
  where removeNewline = T.replace "\n" ""

generateMapFile :: FilePath -> Text -> Map -> IO ()
generateMapFile outDir template map@Map{title, filename} = do
  let mapJSON = decodeUtf8 . BSL.toStrict . encode $ removeNewlines map
      contents = T.replace "$MAP$" mapJSON . T.replace "$TITLE$" title $ template
  T.writeFile (outDir </> filename -<.> "html") contents

ensureDir :: FilePath -> IO ()
ensureDir = createDirectoryIfMissing createParents
  where createParents = True

copyMapFiles :: FilePath -> IO ()
copyMapFiles outDir = do
  let from = "map_files"
  files <- listDirectory from
  forM_ files $ \f -> copyFile (from </> f) (outDir </> f)

generateStaticMapFile :: FilePath -> Map -> IO ()
generateStaticMapFile outDir Map{title, filename, start, finish, extendedMap} = do
  let basename = takeBaseName filename
  mapImages <- filterMapImages basename <$> listDirectory outDir
  writeFile (outDir </> basename <> "_static.html") . renderHtml . staticHTML basename $ categorize mapImages

  where
    filterMapImages basename = filter (\f -> all ($ f) [(basename `isPrefixOf`), (== ".png") . takeExtension])

    categorize = partition ("_gray" `isInfixOf`)

    staticHTML basename (grayscaleImgs, colorImgs) = H.docTypeHtml $ do
      let title' = "Карта: " <> title
      H.head $ do
        H.title $ H.toHtml title'
        H.style "img {max-width: 100%}"
      H.body $ do
        H.h1 $ H.toHtml title'

        showCoord "Старт" start
        showCoord "Финиш" finish
        forM_ extendedMap showExtendedMap
        pageBreak

        images $ sortByZoomLevel basename grayscaleImgs
        images $ sortByZoomLevel basename colorImgs

    images = mapM_ $ \img -> H.p $ H.img ! A.src (H.toValue img)

    sortByZoomLevel basename = sortOn
      ( fmap Down
      . readMaybe @Int
      <=< fmap (takeWhile isDigit)
      . stripPrefix (basename <> "_")
      )

    showCoord :: Text -> Coord -> H.Html
    showCoord name c = H.p $ do
      H.toHtml $ name <> ": "
      H.code . H.toHtml $ mconcat [show $ lat c, ", ", show $ lng c]

    showExtendedMap ExtendedMap{emStartTime, emTimezone, emDistance, emDuration, emElevation, emAvgSpeed, emAvgMovingSpeed, emMaxSpeed} = do
      H.p $ do
        "Время начала: "
        H.code $ do
          H.toHtml $ formatTime defaultTimeLocale "%F %T" emStartTime
          " "
          H.toHtml emTimezone

      H.p $ do
        "Расстояние: "
        H.toHtml $ showDistanceKm emDistance

      H.p $ do
        "Продолжительность: "
        H.code . H.toHtml $ formatTime defaultTimeLocale "%h:%0M:%0S" emDuration

      H.p $ do
        "Скорость: средняя "
        H.toHtml $ showSpeed emAvgSpeed
        ", средняя в движении "
        H.toHtml $ showSpeed emAvgMovingSpeed
        ", максимальная "
        H.toHtml $ showSpeed emMaxSpeed

      showElevation emElevation

    showDistanceKm :: Distance -> H.Html
    showDistanceKm = H.code . H.toHtml @String . printf "%0.2f км" . mToKm . getDistanceMeters
      where mToKm = (/ 1000)

    showSpeed :: Speed -> H.Html
    showSpeed = H.code . H.toHtml @String . printf "%0.2f км/ч" . msToKmh . getSpeedMetersPerSecond
      where msToKmh = (* 3.6)

    showElevation :: Elevation -> H.Html
    showElevation Elevation{elRange, elTotalAscent, elTotalDescent} = H.p $ do
        "Высота: диапазон "
        H.toHtml . showDistanceM $ fst elRange
        " … "
        H.toHtml . showDistanceM $ snd elRange
        ", общий подъём "
        H.toHtml . showDistanceM $ elTotalAscent
        ", общий спуск "
        H.toHtml . showDistanceM $ elTotalDescent

    showDistanceM :: Distance -> H.Html
    showDistanceM = H.code . H.toHtml @String . printf "%0.2f м" . getDistanceMeters

    -- this is needed so that koreader doesn't try to display the image on the
    -- first page since it will be cut off, and the images are designed to fit
    -- full screen
    pageBreak = H.p ! A.style "page-break-after: always;" $ ""

readGarminJSON :: FilePath -> IO Map
readGarminJSON file = do
  summary <- T.readFile file
  let ensureResult = fromMaybe . error . (<> " in " <> file)
      title = fromMaybe "<no title>" $ summary ^? key "activityName" . _String
      filename = takeBaseName file

      summaryValue = ensureResult "no summary value" $ summary ^? key "summaryDTO"
      acceptWhitespace = False
  startTime <- parseTimeM acceptWhitespace defaultTimeLocale "%FT%T.0" . T.unpack
    $ ensureResult "no start time" $ summaryValue ^? key "startTimeLocal" . _String

  let emTimezone = ensureResult "no timezone" $ summary ^? key "timeZoneUnitDTO" . key "timeZone" . _String
      emDistance = Distance . ensureResult "no distance" $ summaryValue ^? key "distance" . _Double
      emDuration = secondsToDiffTime . ensureResult "no distance" $ summaryValue ^? key "distance" . _Integer

      elRange =
        ( Distance . ensureResult "no min elevation" $ summaryValue ^? key "minElevation" . _Double
        , Distance . ensureResult "no max elevation" $ summaryValue ^? key "maxElevation" . _Double
        )
      elTotalAscent = Distance . ensureResult "no total ascent" $ summaryValue ^? key "elevationGain" . _Double
      elTotalDescent = Distance . ensureResult "no total descent" $ summaryValue ^? key "elevationLoss" . _Double
      emElevation = Elevation{elRange, elTotalAscent, elTotalDescent}

      emAvgSpeed = Speed . ensureResult "no avg speed" $ summaryValue ^? key "averageSpeed" . _Double
      emAvgMovingSpeed = Speed . ensureResult "no avg moving speed" $ summaryValue ^? key "averageMovingSpeed" . _Double
      emMaxSpeed = Speed . ensureResult "no max speed" $ summaryValue ^? key "maxSpeed" . _Double

      exMap = ExtendedMap{emStartTime=startTime, emTimezone, emDistance, emDuration, emElevation, emAvgSpeed, emAvgMovingSpeed, emMaxSpeed}

  details <- T.readFile $ dropExtension file <> "_details.json"
  let polylineValue = ensureResult "no polyline value" $ details ^? key "geoPolylineDTO"
      mkCoord p = Coord
        { lat = ensureResult "no lat" $ p ^? key "lat" . _Double
        , lng = ensureResult "no lon" $ p ^? key "lon" . _Double
        }
      start = mkCoord . ensureResult "no start point" $ polylineValue ^? key "startPoint"
      finish = mkCoord . ensureResult "no end point" $ polylineValue ^? key "endPoint"

      polyline = mkCoord <$> ensureResult "no polyline" (polylineValue ^? key "polyline") ^.. values

  pure Map{title, start, finish, filename, sourceMapFilename="", encodedPolylines=Nothing, polyline=Just polyline, extendedMap=Just exMap}

listGarminMapFiles :: FilePath -> IO [FilePath]
listGarminMapFiles dir = fmap (dir </>) . filter isMapFile <$> listDirectory dir
  where
    -- <digits>.json
    isMapFile f = all ($ f)
      [ all isDigit . takeBaseName
      , (== ".json") . takeExtension
      ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--extract", srcDir, "--out", outDir] -> do
      mapFiles <- listMapFiles srcDir
      maps <- traverse parseMapInfo mapFiles
      ensureDir outDir
      BSL.writeFile (outDir </> "maps.json") $ encode maps

    ["--create", mapsFile, "--out", outDir] -> do
      maps <- either (error . ("can't read mapsFile: " <>)) id <$> eitherDecodeFileStrict @[Map] mapsFile
      template <- T.readFile "template.html"
      ensureDir outDir
      copyMapFiles outDir
      forM_ maps $ generateMapFile outDir template

    ["--gen-static", mapsFile, "--out", outDir] -> do
      maps <- either (error . ("can't read mapsFile: " <>)) id <$> eitherDecodeFileStrict @[Map] mapsFile
      forM_ maps $ generateStaticMapFile outDir

    ["--create-garmin", mapsDir, "--out", outDir] -> do
      template <- T.readFile "template.html"
      ensureDir outDir
      copyMapFiles outDir

      mapFiles <- listGarminMapFiles mapsDir
      forM_ mapFiles $ generateMapFile outDir template <=< readGarminJSON

    ["--gen-garmin-static", mapsDir, "--out", outDir] -> do
      mapFiles <- listGarminMapFiles mapsDir
      forM_ mapFiles $ generateStaticMapFile outDir <=< readGarminJSON

    ["--help"] -> do
      name <- getProgName
      putStrLn $ mconcat [name, " (--extract srcDir|--create mapsFile|--gen-static mapsFile|--create-garmin mapsDir|--gen-garmin-static mapsDir) --out outDir"]

    xs -> die $ "can't parse options " <> show xs
