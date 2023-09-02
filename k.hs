#!/usr/bin/env stack
-- stack script --resolver lts-21.8

import Control.Monad
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process

main :: IO ()
main = do
  epub <- getFile
  dir <- extractEPUB epub
  let newEPUB = epub -<.> ".new.epub"
  createEPUB dir newEPUB

getFile :: IO EPUBFile
getFile = do
  args <- getArgs
  case args of
    [file] -> pure file
    _ -> die "k.hs <INPUT_EPUB>"

-- EPUB processing

-- | Extracts the epub file into a directory with the base name of the file, and
-- returns the created directory.
extractEPUB :: EPUBFile -> IO FilePath
extractEPUB file = do
  let dir = dropExtension file
  out <- readProcess "unzip" ["-qo", file, "-d", dir] ""
  when (not $ null out) $
    putStrLn $ mconcat ["extract ", file, ":\n", out]

  pure dir

type EPUBFile = FilePath

-- | Creates an epub file from the directory.
--
-- Based on `https://ebooks.stackexchange.com/questions/257/how-to-repack-an-epub-file-from-command-line/6171#6171`
createEPUB :: FilePath -> EPUBFile -> IO ()
createEPUB dir epub = do
  -- in order to have files directly in the archive (as is required by the format),
  -- we need to `zip` in that directory, which means the target `epub` file,
  -- when relative, will point to the wrong place (within the new working dir),
  -- so we to make the `epub` filepath absolute first
  absEPUB <- makeAbsolute epub
  -- wow! even though `man zip` mentions `--no-extra`:
  -- `zip error: Invalid command arguments (long option 'no-extra' not supported)`!
  -- (zip 3.0)
  let zip0 = (proc "zip" ["-q0X", absEPUB, "mimetype"]) { cwd = Just dir }
  out <- readCreateProcess zip0 ""
  when (not $ null out) $
    putStrLn $ mconcat ["zip0 ", absEPUB, ":\n", out]
