{-# LANGUAGE DerivingStrategies, ImportQualifiedPost, NamedFieldPuns, OverloadedStrings #-}

import Data.List (isPrefixOf, find)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe, fromJust)
import Data.Text (Text)
import Data.Text qualified as T (strip, pack, isInfixOf, unpack, split)
import Data.Text.IO qualified as T (readFile)
import Language.ECMAScript3 (Expression(..), Id(..), JavaScript, PrefixOp(..), Prop(..), SourcePos, Statement(..), VarDecl(..), parse, program, unJavaScript)
import System.Environment (getArgs)
import System.FilePath (takeFileName)
import Text.HTML.TagSoup (Tag(..), innerText, parseTags)
import Text.HTML.TagSoup.Match (tagComment)

-- | WGS84 geographic coordinate.
data Coord = Coord { lat :: !Double, long :: !Double }
  deriving (Show)

-- | Information about a Kitya's track on the map.
data Map = Map
  { filename :: !FilePath
  -- ^ filename of the mirrored HTML file
  , sourceMapFilename :: !FilePath
  -- ^ filename of the source text file with map data, extracted from the source
  -- URL, which is recorded by HTTrack
  , title :: !Text
  , start :: !Coord
  , finish :: !Coord
  , encodedPolylines :: !(NonEmpty Text)
  }
  deriving (Show)

parseMapInfo :: FilePath -> IO Map
parseMapInfo f = do
  t <- T.readFile f
  let tags = parseTags t
      title = T.strip . innerText . take 2 . dropWhile (/= TagOpen "title" []) $ tags
      (start, finish, encodedPolylines) = extractTrack tags
      filename = takeFileName f
      sourceMapFilename = extractSourceMapFilename tags
  pure Map{filename, sourceMapFilename, title, start, finish, encodedPolylines}

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
    markerCoord [latE, longE] = Coord{lat=evalExpr latE, long=evalExpr longE}
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

main :: IO ()
main = do
  srcFile <- head <$> getArgs
  print =<< parseMapInfo srcFile
