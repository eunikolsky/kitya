#!/usr/bin/env stack
-- stack script --resolver lts-19.31 --package base,hxt,split,directory,filepath,text,css-text,time,process --ghc-options=-hide-all-packages

-- note: add this later before running for real: --optimize

-- note: the `--package` argument above doesn't work when loading the script into `stack ghci`; pass
-- them manually, each after its own `--package` argument

{-# LANGUAGE NamedFieldPuns, OverloadedStrings, RecordWildCards, TypeApplications #-}
{-# OPTIONS_GHC -Wall -Werror=missing-methods -Werror=missing-fields #-}

module Kitya where

import Control.Exception (bracket, finally)
import Control.Monad (filterM, unless, void)
import Data.Char (isDigit)
import Data.Either (fromRight)
import Data.Foldable (for_, traverse_)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (intercalate, intersperse, isInfixOf, isPrefixOf, singleton, sortOn, uncons)
import Data.List.Split (dropFinalBlank, dropInitBlank, dropInnerBlanks, keepDelimsL, split, whenElt)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (utcToLocalZonedTime)
import Prelude hiding (log)
import Text.CSS.Parse
import Text.CSS.Render
import Text.XML.HXT.Core
import System.Directory (createDirectory, doesDirectoryExist, getModificationTime, getPermissions, listDirectory, renameDirectory, setModificationTime, setPermissions, withCurrentDirectory, writable)
import System.FilePath ((</>), (<.>), dropExtension, splitExtension)
import System.Environment (getArgs)
import System.Exit (die)
import System.Process (readProcess)
import qualified Data.Text as T (pack)
import qualified Data.Text.Lazy as TL (unpack)
import qualified Data.Text.Lazy.Builder as TB (toLazyText)
import qualified Text.XML.HXT.DOM.XmlNode as XN

main :: IO ()
main = getProgramArgs >>= createBooks

newtype InputArgs = InputArgs { iaDate :: Maybe (Year, Month) }

-- | The program's arguments.
data Args = Args
  { arOutputDir :: !FilePath
  , arInputArgs :: !InputArgs
  }

getProgramArgs :: IO Args
getProgramArgs = do
  args <- getArgs
  case args of
    ["-i", year, month, arOutputDir] -> do
      exists <- doesDirectoryExist arOutputDir
      unless exists $ createDirectory arOutputDir
      pure $ Args { arOutputDir, arInputArgs = InputArgs $ Just (year, month) }

    [arOutputDir] -> do
      exists <- doesDirectoryExist arOutputDir
      unless exists $ createDirectory arOutputDir
      pure $ Args { arOutputDir, arInputArgs = InputArgs Nothing }

    _ -> die "Usage: kitya [-i <YEAR> <MONTH>] <OUTPUT_DIR>"

-- |Main function to recursively go through the blog hierarchy and create epubs.
createBooks :: Args -> IO ()
createBooks Args { arOutputDir = outputDir, arInputArgs = InputArgs iaDate } = do
  -- I tried using `StateT Int IO` at first, but it means I had to switch to
  -- `bracket` and `MonadUnliftIO` from `unliftio`, and the package doesn't
  -- provide an `instance MonadUnliftIO (StateT s m)`; thus I resorted to using
  -- plain IO and mutable references in IO — this is fine in this small script,
  -- but does suck in general
  bookNumberRef <- newIORef 0
  forYearMonth iaDate $ \(year, month) -> do
    putStrLn $ mconcat ["Processing ", year, "/", month, "…"]

    posts <- listPosts "."
    amendHTMLs posts
    generateIndex posts

    nowString <- getNowString
    bookNumber <- readIORef bookNumberRef

    createEpub outputDir $ EpubSettings
      { yearMonth = year <> "-" <> month
      , nowString
      , bookNumber
      }

    modifyIORef' bookNumberRef (+ 1)

  where
    -- picks between the requested year-month or all the discovered ones
    forYearMonth Nothing = forEachYearMonth
    forYearMonth (Just ym) = forGivenYearMonth ym

    amendHTMLs posts =
      let postPairs = adjacentPairs posts
      in for_ postPairs $
        \(file, next) -> withWriteableFile file $ amendHTML file next

type Year = String
type Month = String
type YearMonthFunc = (Year, Month) -> IO ()

forEachYearMonth :: YearMonthFunc -> IO ()
forEachYearMonth f =
  withWriteableFilePreservingModTime "." $

  forEachYear $ \year ->
  withWriteableFilePreservingModTime year $
  withCurrentDirectory year $

  forEachMonth $ \month ->
  withWriteableFilePreservingModTime month $
  withBackedupCurrentDirectory month $

    f (year, month)

  where
    forEachNumericDir f' = listNumericDirs >>= traverse_ f'
    listNumericDirs = fmap (sortOn $ read @Int) . filterM doesDirectoryExist =<< filter (all isDigit) <$> listDirectory "."
    forEachYear = forEachNumericDir
    forEachMonth = forEachNumericDir

-- this is a simplified copy of `forEachYearMonth` that looks at only one
-- year-month; I'm not sure how to combine these two w/o introducing unnecessary
-- filesystem operations when we need only one year-month
forGivenYearMonth :: (Year, Month) -> YearMonthFunc -> IO ()
forGivenYearMonth (year, month) f =
  withWriteableFilePreservingModTime "." $

  withWriteableFilePreservingModTime year $
  withCurrentDirectory year $

  withWriteableFilePreservingModTime month $
  withBackedupCurrentDirectory month $

    f (year, month)

data EpubSettings = EpubSettings
  { yearMonth :: !String -- ^ Year and month from the filesystem, e.g. `2004-01`.
  , nowString :: !String -- ^ Current time for book publication.
  , bookNumber :: !Int -- ^ The number of the book overall.
  }
  deriving (Show)

-- ebook-convert index.html kitya.epub --breadth-first --max-levels 1 --chapter '/' --page-breaks-before '/' --level1-toc '//h:h1' --level2-toc '//h:h2' --authors 'Китя Карлсон' --book-producer egeek --language Russian --pubdate "$( date '+%FT%T%z' )" --series 'Блог Кити Карлсона' --title 'Китя Карлсон, 2004-01' --flow-size 0
createEpub :: FilePath -> EpubSettings -> IO ()
createEpub outputDir EpubSettings{..} = readProcess' converter args
  where
    converter = "ebook-convert"
    args =
      [ "index.html"
      , outputDir </> "kitya_" <> yearMonth <.> "epub"

      -- https://manual.calibre-ebook.com/generated/en/ebook-convert.html#html-input-options
      , "--breadth-first"
      , "--max-levels", "1"
      , "--allow-local-files-outside-root"

      -- https://manual.calibre-ebook.com/generated/en/ebook-convert.html#epub-output-options
      , "--flow-size", "0"
      , "--preserve-cover-aspect-ratio"

      -- https://manual.calibre-ebook.com/generated/en/ebook-convert.html#structure-detection
      , "--chapter", "/"
      , "--page-breaks-before", "/"

      -- https://manual.calibre-ebook.com/generated/en/ebook-convert.html#table-of-contents
      , "--level1-toc", "//h:h1"
      , "--level2-toc", "//h:h2"

      -- https://manual.calibre-ebook.com/generated/en/ebook-convert.html#metadata
      , "--authors", "Китя Карлсон"
      , "--book-producer", "egeek"
      , "--language", "Russian"
      , "--pubdate", nowString
      , "--series", "Блог Кити Карлсона"
      , "--series-index", show bookNumber
      , "--tags", "blog"
      , "--title", "Китя Карлсон, " <> yearMonth
      ]

readProcess' :: FilePath -> [String] -> IO ()
readProcess' exe args = log >> run
  where
    run = readProcess exe args stdin >>= putStrLn
    stdin = ""
    log = putStrLn . intercalate " " . (["$", exe] <>) . map (\s -> "\"" <> s <> "\"") $ args

-- === Month processing

listPosts :: FilePath -> IO [FilePath]
listPosts dir = sortOn (read @Int . dropExtension)
  . filter (\fp -> let (fname, ext) = splitExtension fp in ext == ".html" && all isDigit fname)
  <$> listDirectory dir

generateIndex :: [FilePath] -> IO ()
generateIndex files = void . runX $
  doc >>> indentDoc >>> writeDocument [withOutputXHTML, withAddDefaultDTD yes, withXmlPi no] "index.html"
  where
    doc = root [] $ link <$> files
    link file = aelem "a" [sattr "href" file]

-- === Helpers

adjacentPairs :: [a] -> [(a, Maybe a)]
adjacentPairs [] = []
adjacentPairs xs = zip xs (Just <$> tail xs) ++ [(last xs, Nothing)]

getNowString :: IO String
getNowString = fmap (formatTime defaultTimeLocale "%FT%T%z") . utcToLocalZonedTime =<< getCurrentTime

-- === Filesystem exception-safe wrappers

withWriteableFilePreservingModTime :: FilePath -> IO a -> IO a
withWriteableFilePreservingModTime fp f = bracket
  ((,) <$> getModificationTime fp <*> getPermissions fp)
  (\(time, perm) -> setPermissions fp perm >> setModificationTime fp time)
  (\(_, perm) -> setPermissions fp (perm { writable = True }) >> f)

withBackedupCurrentDirectory :: FilePath -> IO a -> IO a
withBackedupCurrentDirectory dir f =
  (backupDir >> withCurrentDirectory dir f)
  `finally`
  removeBackupDir

  where
    backupDir = readProcess' "cp" ["-pr", dir, originalDir]
    removeBackupDir = readProcess' "trash" [dir] >> renameDirectory originalDir dir
    originalDir = dir <> ".orig"

withWriteableFile :: FilePath -> IO a -> IO a
withWriteableFile fp f = bracket
  (getPermissions fp)
  (setPermissions fp)
  (\perm -> setPermissions fp (perm { writable = True }) >> f)

--withPreservingModTime :: FilePath -> IO a -> IO a
--withPreservingModTime fp f = bracket (getModificationTime fp) (setModificationTime fp) (const f)

-- === XML processing

-- Fucking A!
amendHTML :: FilePath -> Maybe FilePath -> IO ()
amendHTML file nextFile = void . runX $ load >>> process >>> save
  where
    load = readDocument [withParseHTML yes, withWarnings no, withPreserveComment yes] file
    process = seqA
      [ movePostHeaderBeforeDate
      , removeMenu
      , removePreCommentsTable
      , downgradeCommentsHeader
      , fixHTMLNewlinesInComments
      , treeizeComments maxCommentLevel nextFile
      , removeBodyInComments
      -- warning: `wrapDate` has to be after `removeBodyInComments` because it
      -- wraps tagless text in `body` tags with a `div`, and we don't need extra
      -- `div`s in comments
      , wrapDate
      , editStyles
      , removeLinksToImages
      -- warning: `removeCommentersProfileLinks` is tested to be after
      -- `removeLinksToImages` (but it may also work before)
      , removeCommentersProfileLinks
      ]
    save = writeDocument [withOutputXHTML, withAddDefaultDTD yes, withXmlPi no] file

    -- | Too deeply nested comment chains stop being readable on an e-reader w/o
    -- horizontal scrolling because they go off-screen due to the too large
    -- accumulated left margin. So this constant defines the maximum comment
    -- nesting level, after which comments are skipped.
    maxCommentLevel :: MaxLevel
    maxCommentLevel = 99

movePostHeaderBeforeDate :: ArrowXml a => a XmlTree XmlTree
movePostHeaderBeforeDate = processTopDown $
  (insertChildrenAt 0 (deep (hasName "h1") >>. take 1) `when` hasName "body") >>>
  (ifA (hasName "div" >>> hasAttrValue "id" (== "text")) (processChildren . filterA . neg $ hasName "h1") this)

removeMenu :: ArrowXml a => a XmlTree XmlTree
removeMenu = processTopDown (filterA . neg $ hasName "ul" >>> hasAttrValue "id" (== "menu"))

wrapDate :: ArrowXml a => a XmlTree XmlTree
wrapDate = processTopDown $
  processChildren (selem "div" [this] `when` isText) `when` hasName "body"

removePreCommentsTable :: ArrowXml a => a XmlTree XmlTree
removePreCommentsTable = processTopDown (filterA . neg $ hasName "table" //> statImg)
  where
    statImg = (hasName "img") >>> hasAttrValue "src" (== "http://www.karlson.ru/lj/stat.php")

downgradeCommentsHeader :: ArrowXml a => a XmlTree XmlTree
downgradeCommentsHeader = processTopDown (setElemName (mkName "h2") `when` (hasName "h1" >>> deep (hasText ("Комментарии" `isPrefixOf`))))

commentsDiv :: ArrowXml a => a XmlTree XmlTree
commentsDiv = hasName "div" >>> hasAttrValue "id" (== "comm")

commentSubject :: ArrowXml a => a XmlTree XmlTree
commentSubject = hasAttrValue "class" (== "comment_subject")

-- |Removes unnecessary (and causing visual changes for `ebook-convert`) `<body>`
-- tags inside comments. The comment body is already wrapped in a `<div>`.
removeBodyInComments :: ArrowXml a => a XmlTree XmlTree
removeBodyInComments = processTopDown $ removeBodyTags `when` commentsDiv
  where
    removeBodyTags = processTopDown $ getChildren `when` hasName "body"

-- TODO add next post links could be extracted from here
treeizeComments :: ArrowXml a => MaxLevel -> Maybe FilePath -> a XmlTree XmlTree
treeizeComments maxCommentLevel nextPostLink = processTopDown ( (replaceChildren ( leaveHeader <+> addLinks ) `when` commentsDiv) >>> tweakStyles )
  where
    addLink = replaceChildren (getChildren <+> link nextPostLink)
    addLinks = doTreeizeComments >>> processChildren (addLink `when` commentSubject)
    link src = mkelem name (sattr "class" "next_post_link" : attrs) [txt . textEscapeXml $ text]
      where
        name = maybe "span" (const "a") src
        attrs = maybe [] (singleton . sattr "href") src
        text = maybe "|||||" (const ">>>>>") src
    -- note: the parenthesis around the expression before `>>.` are extremely important! the code will
    -- run without them, but it won't do what's needed: here it groups all the modified `div`s into a
    -- list and passes it to `treeize`; w/o the parenthesis, `treeize` would be called multiple times
    -- with a singleton list evey time.
    doTreeizeComments =
      (
        getChildren
          >>> hasName "div"
          >>>
            (
              (
                (getAttrValue0 "style" >>. map levelFromStyle)
                  &&& removeAttr "style"
              )
              >>> arr (uncurry LXmlTree)
            )
      ) >>. treeize maxCommentLevel

    leaveHeader = getChildren >>> hasNameWith (("h" `isPrefixOf`) . localPart)

    tweakStyles = processTopDown (changeText $ appendCommentStyles . removeVerticalMargins) `when` hasName "style"
      where
        removeVerticalMargins = unlines
          . filter (not . uncurry (||) . (isInfixOf "margin-top" &&& isInfixOf "margin-bottom"))
          . lines
        appendCommentStyles = flip (<>) . unlines $
          [ ".comment { margin-left: 0.5em; }"
          , "#comm > .comment { margin-left: 0; }"
          , ".next_post_link { font-weight: bold; float: right; color: red; }"
          ]

editStyles :: ArrowXml a => a XmlTree XmlTree
editStyles = processTopDownUntil $ hasName "style" `guards` processChildren (changeText $ append . remove)
  where
    append = (<> "date { display: inline; /* for koreader */ }\n")

    -- margin-left
    removeCommentsStyles = filter ((/= ".comments") . selector)
    leaveOnlyBorderBottomStyleForBody = map (\block ->
        (if selector block == ".body"
          then second (filter $ (== "border-bottom") . attrName)
          else id)
        block)

    selector = fst
    attrName = fst

    remove = TL.unpack
      . TB.toLazyText
      . renderBlocks
      . leaveOnlyBorderBottomStyleForBody
      . removeCommentsStyles
      . fromRight []
      . parseBlocks
      . T.pack

-- |Replaces links to images with those images themselves, that is:
-- `<a href="full.jpg"><img src="thumbnail.jpg" alt="" width="25" height="25" /></a>`
-- is replaced with:
-- `<img src="full.jpg" alt="" />`.
--
-- This is to fix a warning produced by Calibre's ebook-edit:
-- "The link "thumbnail.jpg" points to a file thumbnail.jpg that is not a text (HTML) document. Many e-book readers will be unable to follow such a link. You should either remove the link or change it to point to a text document. For example, if it points to an image, you can create small wrapper document that contains the image and change the link to point to that."
-- Note that if you "Try to correct all fixable errors automatically" in the editor,
-- it will simply remove such links with images!
removeLinksToImages :: ArrowXml a => a XmlTree XmlTree
removeLinksToImages = processTopDown $
  ((getChildren >>> imageWithoutDimensions) += imageSourceFromLink) `when` imageLink
  where
    imageWithoutDimensions = hasName "img" >>> removeAttr "width" >>> removeAttr "height"
    imageSourceFromLink = mkAttr (mkName "src") (getAttrValue0 "href" >>> mkText)
    imageLink = hasName "a" /> hasName "img"

-- | Inserts `<br/>` before every `\n` in a comment text, otherwise all comments
-- are displayed w/o any intended line breaks. Note: livejournal's comments do
-- have newlines and `<br/>` tags in comments, but Kitya's lj replicator lost
-- the breaks.
fixHTMLNewlinesInComments :: ArrowXml a => a XmlTree XmlTree
fixHTMLNewlinesInComments = processTopDown $ processTopDown mapNode `when` commentBody
  where
    mapNode = (getText >>. insertBreaks) `when` isText

    insertBreaks :: [String] -> [XmlTree]
    insertBreaks [s] = intersperse br $ XN.mkText <$> splitBeforeNewlines s
    insertBreaks xs = error $ "Expected to have single text node, but got " <> show xs

    -- `splitBeforeNewlines "foo\n\nbar\nbaz" = ["foo", "\n", "\nbar", "\nbaz"]`
    splitBeforeNewlines :: String -> [String]
    splitBeforeNewlines = split $ keepDelimsL (whenElt (== '\n'))

    commentBody = hasAttrValue "class" (== "comment_body")
    br = XN.mkElement (mkName "br") [] []

-- | Remove useless links to commenter's profiles. As there can be many of them
-- on the left side of the page, the user may unintentionally tap on a link
-- instead of scrolling the page.
removeCommentersProfileLinks :: ArrowXml a => a XmlTree XmlTree
removeCommentersProfileLinks = processTopDown $ removeLinks `when` commentSubject
  where
    removeLinks = processTopDown $ getChildren `when` profileLink
    profileLink = hasName "a" >>> hasAttrValue "href" (".livejournal.com" `isInfixOf`)


type Level = Int
-- |`XmlTree` (in our case, a `div` element containing a comment) with its level extracted from the style;
-- the level is extracted beforehand so that the style doesn't need to be parsed again and again.
data LXmlTree = LXmlTree
  { ltLevel :: !Level
  , ltTree :: !XmlTree
  }
  deriving (Show)

type MaxLevel = Int

-- Fuckng AA!!
-- TODO move `maxLevel` processing out of here; I added it here for simplicity
-- because the level is already known at the moment
treeize :: MaxLevel -> [LXmlTree] -> XmlTrees
treeize maxLevel = lforest 0
  where
    lforest :: Int -> [LXmlTree] -> XmlTrees
    lforest level xs = ltree level <$> sublistsFromLevel level xs

    ltree :: Int -> [LXmlTree] -> XmlTree
    ltree level xs = case uncons xs of
      Just (LXmlTree _ (XN.NTree root' children), rest) ->
        let nextLevel = level + 1
            nestedLevels = if nextLevel <= maxLevel
              then lforest nextLevel rest
              else []
        in XN.NTree root' $ children <> nestedLevels
      Nothing -> error $ mconcat ["treeize: unexpected empty LXmlTree at level ", show level]

levelFromStyle :: String -> Level
levelFromStyle = fromMargin . read @Int . takeWhile isDigit . dropWhile (not . isDigit)
  where
    fromMargin leftMargin = let (level', rem') = leftMargin `divMod` 30
      in if rem' == 0
        then level'
        else error (mconcat ["Unexpected left margin " <> show leftMargin <> ", expected divisible by 30"])

sublistsFromLevel :: Level -> [LXmlTree] -> [[LXmlTree]]
sublistsFromLevel l = split (dropInitBlank . dropInnerBlanks . dropFinalBlank . keepDelimsL $ whenElt ((== l) . ltLevel))
