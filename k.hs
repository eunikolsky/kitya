#!/usr/bin/env stack
-- stack script --resolver lts-21.8

{-# OPTIONS_GHC -Wall #-}

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
  absEPUB <- makeAbsolute epub

  -- wow! even though `man zip` mentions `--no-extra`:
  -- `zip error: Invalid command arguments (long option 'no-extra' not supported)`!
  -- (zip 3.0)
  runProc $ (proc "zip" ["-q0X", absEPUB, fileMimetype]) { cwd = Just dir }

  -- `META-INF/` should be the second file in the archive
  runProc $ (proc "zip" $ ["-q9Xgr", absEPUB, fileMetaInf] <> contents) { cwd = Just dir }

-- | Runs the given `CreateProcess` with an empty `stdin`; prints the returned
-- `stdout` and/or `stderr` if non-empty; expects a successful exit code,
-- terminating the program otherwise.
runProc :: CreateProcess -> IO ()
runProc p = do
  let cmd = mconcat ["[", show $ cmdspec p, "]"]
  (exitCode, out, err) <- readCreateProcessWithExitCode p ""

  when (not $ null out) $ putStrLn $ mconcat [cmd, " out:\n", out]
  when (not $ null err) $ putStrLn $ mconcat [cmd, " err:\n", err]

  unless (exitCode == ExitSuccess) $
    die $ mconcat [cmd, " exit code ", show exitCode, " != 0"]

