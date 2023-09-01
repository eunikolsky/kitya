#!/usr/bin/env stack
-- stack script --resolver lts-21.8

import Control.Monad
import System.Environment
import System.Exit
import System.FilePath
import System.Process

main :: IO ()
main = getFile >>= extractEPUB

getFile :: IO FilePath
getFile = do
  args <- getArgs
  case args of
    [file] -> pure file
    _ -> die "k.hs <INPUT_EPUB>"

-- EPUB processing

-- | Extracts the epub file into a directory with the base name of the file.
extractEPUB :: FilePath -> IO ()
extractEPUB file = do
  let dir = dropExtension file
  out <- readProcess "unzip" ["-qo", file, "-d", dir] ""
  when (not $ null out) $
    putStrLn $ mconcat ["extract ", file, ":\n", out]
