{-# LANGUAGE DerivingStrategies, ImportQualifiedPost, OverloadedStrings #-}

import Data.Text (Text)
import Data.Text qualified as T (strip)
import Data.Text.IO qualified as T (readFile)
import Language.ECMAScript3
import System.Environment (getArgs)
import Text.HTML.TagSoup (Tag(..), innerText, parseTags)
import Data.Maybe (mapMaybe)

-- | WGS84 geographic coordinate.
data Coord = Coord { lat :: !Double, long :: !Double }
  deriving (Show)

-- | Information about a Kitya's track on the map.
data Map = Map
  { title :: !Text
  , start :: !Coord
  }
  deriving (Show)

parseMapInfo :: FilePath -> IO Map
parseMapInfo f = do
  t <- T.readFile f
  let tags = parseTags t
      title = T.strip . innerText . take 2 . dropWhile (/= TagOpen "title" []) $ tags
      start = extractStart tags
  pure Map{title, start}

getJSText :: [Tag Text] -> Text
getJSText = innerText . take 2 . dropWhile (/= TagOpen "script" [("type", "text/javascript")])

parseJS :: Text -> JavaScript SourcePos
parseJS = either (error . ("couldn't parse: " <>) . show) id . parse program ""

extractStart :: [Tag Text] -> Coord
extractStart = markerCoord . findStart . parseJS . getJSText
  where
    {- note: looking for the necessary things in the AST is a pain (I wonder if
     - there is something HXT-like to use arrows for tree processing?), so I
     - dumped the AST of one map file and used it to find where the interesting
     - things are; all the `index.{4}\.html` files have the same structure,
     - although with a varying number of `encodedPoints_*` definitions, so it's
     - fine to have a rigid parser; the files aren't going to change anyway
     -}
    findStart = concat . concatMap (mapMaybe startVar) . concatMap (mapMaybe gBrowserIsCompatibleIf) . mapMaybe initializeFunc . unJavaScript
    initializeFunc (FunctionStmt _ (Id _ "initialize") _ ss) = Just ss
    initializeFunc _ = Nothing
    gBrowserIsCompatibleIf (IfSingleStmt _ (CallExpr _ (VarRef _ (Id _ "GBrowserIsCompatible")) []) (BlockStmt _ ss)) = Just ss
    gBrowserIsCompatibleIf _ = Nothing
    startVar (VarDeclStmt _ [VarDecl _ (Id _ "s") (Just (NewExpr _ (VarRef _ (Id _ "GMarker")) [NewExpr _ (VarRef _ (Id _ "GLatLng")) ss, ObjectLit _ [(PropId _ (Id _ "title"), StringLit _ "start")]]))]) = Just ss
    startVar _ = Nothing

    -- [NumLit _ 46.126683333333,PrefixExpr _ PrefixMinus (NumLit _ 121.51863333333)]
    markerCoord :: Show a => [Expression a] -> Coord
    markerCoord [latE, longE] = Coord{lat=evalExpr latE, long=evalExpr longE}
    markerCoord u = error $ "unexpected coords " <> show u

    evalExpr (NumLit _ x) = x
    evalExpr (PrefixExpr _ PrefixMinus (NumLit _ x)) = -x
    evalExpr u = error $ "unexpected expression " <> show u

main :: IO ()
main = do
  srcFile <- head <$> getArgs
  print =<< parseMapInfo srcFile
