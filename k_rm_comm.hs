#!/usr/bin/env stack
-- stack script --optimize --resolver lts-21.8 --ghc-options=-threaded --ghc-options=-rtsopts --ghc-options=-with-rtsopts=-N --package async,directory,filepath,hxt,process,time

-- note: `--optimize` (or `--compile`) is required to compile the program so
-- that `-threaded` works for the concurrent runtime support!
-- See: https://stackoverflow.com/questions/46311648/multicore-parallelism-with-stack-runghc

-- License: GNU GPL v3.0 (see `license`)

{-# OPTIONS_GHC -Wall #-}

import Control.Concurrent.Async
import Control.Concurrent.QSem
import Control.Exception
import Control.Monad qualified as M
import Data.Time.Clock
import Data.Time.Format.ISO8601
import Data.Time.LocalTime
import GHC.Conc
import System.Directory
import System.Exit
import System.FilePath
import System.Process
import Text.XML.HXT.Core hiding (err)

main :: IO ()
main = do
  numProc <- getNumCapabilities

  epubs <- findEPUBs
  outDir <- ensureDir "out"
  mapPool numProc (removeCommentsEPUB outDir) epubs

-- | Remove comments from the given epub, placing the new epub into the `newDir`.
removeCommentsEPUB :: FilePath -> EPUBFile -> IO ()
removeCommentsEPUB newDir epub = do
  dir <- extractEPUB epub

  removeCommentsInAllBlogposts dir
  leaveTopLevelTOCEntries $ dir </> "toc.ncx"
  removeIndexFile dir
  modifyContent $ dir </> "content.opf"

  let newEPUB = newDir </> epub -<.> ".no_comm.epub"
  createEPUB dir newEPUB

  removeDirectoryRecursive dir

-- | Returns all `.epub` files in the current directory.
findEPUBs :: IO [EPUBFile]
findEPUBs = filter ((== ".epub") . takeExtension) <$> listDirectory "."

-- | Creates `dir` if it doesn't exist yet.
ensureDir :: FilePath -> IO FilePath
ensureDir dir = do
  exists <- doesDirectoryExist dir
  M.unless exists $ createDirectory dir
  pure dir

-- | Maps the actions to up to `n` concurrent threads.
--
-- Based on https://stackoverflow.com/a/18898822
mapPool :: Traversable t => Int -> (a -> IO ()) -> t a -> IO ()
mapPool n f xs = do
  sem <- newQSem n
  -- `mapConcurrently_`, not `traverse_`!
  mapConcurrently_ (\x -> bracket_ (waitQSem sem) (signalQSem sem) (f x)) xs

-- EPUB processing

-- | Extracts the epub file into a directory with the base name of the file, and
-- returns the created directory.
extractEPUB :: EPUBFile -> IO FilePath
extractEPUB file = do
  let dir = dropExtension file
  runProc $ proc "unzip" ["-qo", file, "-d", dir]

  pure dir

type EPUBFile = FilePath

-- | Creates an epub file from the directory.
--
-- Based on `https://ebooks.stackexchange.com/questions/257/how-to-repack-an-epub-file-from-command-line/6171#6171`
createEPUB :: FilePath -> EPUBFile -> IO ()
createEPUB dir epub = do
  -- these files must be present in an epub
  let fileMimetype = "mimetype"
      fileMetaInf = "META-INF"

  -- we're listing the files before creating an epub in case the new file is in
  -- the directory (an unsupported case) so that it's not included
  contents <- filter (\f ->
      f /= fileMimetype && f /= fileMetaInf
    ) <$> listDirectory dir

  -- in order to have files directly in the archive (as is required by the format),
  -- we need to `zip` in that directory, which means the target `epub` file,
  -- when relative, will point to the wrong place (within the new working dir),
  -- so we to make the `epub` filepath absolute first
  -- an alternative would be to change the working directory in the program here,
  -- but that would make the function not concurrent
  absEPUB <- makeAbsolute epub

  -- wow! even though `man zip` mentions `--no-extra`:
  -- `zip error: Invalid command arguments (long option 'no-extra' not supported)`!
  -- (zip 3.0)
  runProc $ (proc "zip" ["--quiet", "-0X", absEPUB, fileMimetype]) { cwd = Just dir }

  -- `META-INF/` should be the second file in the archive
  -- (`--recursive-paths` isn't supported either)
  runProc $ (proc "zip" $ ["--quiet", "--grow", "-9Xr", absEPUB, fileMetaInf] <> contents) { cwd = Just dir }

-- | Runs the given `CreateProcess` with an empty `stdin`; prints the returned
-- `stdout` and/or `stderr` if non-empty; expects a successful exit code,
-- terminating the program otherwise.
runProc :: CreateProcess -> IO ()
runProc p = do
  let cmd = mconcat ["[", show $ cmdspec p, "]"]
  (exitCode, out, err) <- readCreateProcessWithExitCode p ""

  M.when (not $ null out) $ putStrLn $ mconcat [cmd, " out:\n", out]
  M.when (not $ null err) $ putStrLn $ mconcat [cmd, " err:\n", err]

  M.unless (exitCode == ExitSuccess) $
    die $ mconcat [cmd, " exit code ", show exitCode, " != 0"]

-- Contents processing

processXML ::
  -- ArrowXml a => a XmlTree XmlTree
  IOSLA (XIOState ()) XmlTree XmlTree
  -> FilePath -> IO ()
processXML process f = M.void . runX $
  readDocument [withValidate no] f
  >>> process
  >>> writeDocument [] f

-- | Removes the second level of entries, corresponding to the comments, from
-- the TOC file.
--
-- `playOrder` will have every other number now, but it shouldn't matter.
leaveTopLevelTOCEntries :: FilePath -> IO ()
leaveTopLevelTOCEntries = processXML $
  processTopDownUntil ((hasName "navPoint") `guards` (processChildren $ none `when` (hasName "navPoint")))

-- | Removes comments in all the HTML files in the given directory.
removeCommentsInAllBlogposts :: FilePath -> IO ()
removeCommentsInAllBlogposts dir = do
  allFiles <- fmap (dir </>) <$> listDirectory dir
  let htmls = filter ((== ".html") . takeExtension) allFiles
  M.forM_ htmls removeCommentsBlogpost

-- | Removes the comments section from the HTML blogpost file.
removeCommentsBlogpost :: FilePath -> IO ()
removeCommentsBlogpost = processXML $
  -- this leaves the `<div id="comm"></div>` itself
  processTopDownUntil (hasAttrValue "id" (== "comm") `guards` (replaceChildren none))

-- | Modifies the `content.opf` file:
-- * removes `index.html`;
-- * amends the title;
-- * refreshes the create time to `now`.
modifyContent :: FilePath -> IO ()
modifyContent f = do
  now <- getCurrentTime
  flip processXML f $
    removeIndexFileEntry >>> updateTitle >>> refreshTime now

-- | Removes the `index.html` file from the contents list; it only added an
-- empty page in the book.
removeIndexFileEntry :: ArrowXml a => a XmlTree XmlTree
removeIndexFileEntry =
  processTopDown (filterA $ neg $ hasName "item" >>> hasAttrValue "href" (== "index.html"))

-- | Appends " (без комментариев)" to the book title.
updateTitle :: ArrowXml a => a XmlTree XmlTree
updateTitle = processTopDown $
  -- I don't understand why this nested `processTopDown` is necessary because
  -- `XText` is a child of `XTag`
  processTopDown (changeText (<> " (без комментариев)"))
    `when` hasName "dc:title"

-- | Refreshes the creation time in two places to the given time.
refreshTime :: ArrowXml a => UTCTime -> a XmlTree XmlTree
refreshTime time = processTopDown . seqA $
  [ processTopDown (changeText (const showTimeNoMs)) `when` hasName "dc:date"
  , addAttr "content" showTime
      `when` (hasName "meta" >>> hasAttrValue "name" (== "calibre:timestamp"))
  ]

  where
    timeNoMs = time { utctDayTime = fromIntegral @Integer . floor $ utctDayTime time }
    -- formatting `ZonedTime` here because it shows the timezone as `+00:00`,
    -- like in the book, whereas `UTCTime` shows it as `Z`
    showTimeNoMs = iso8601Show $ utcToZonedTime utc timeNoMs

    showTime = iso8601Show $ utcToZonedTime utc time

-- | Removes `index.html` in the given directory.
removeIndexFile :: FilePath -> IO ()
removeIndexFile = removeFile . (</> "index.html")
