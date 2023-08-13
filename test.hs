#!/usr/bin/env stack
-- stack script --resolver lts-19.31 --package base,hxt,split,directory,filepath,text,css-text,time,process,hspec --ghc-options=-hide-all-packages

module Test where

import Control.Monad
import Kitya hiding (main)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "levelFromStyle" $
    forM_ [("margin-left: 00px;", 0), ("margin-left: 120px;", 4)] $ \(style, level) ->
      it ("parses level from style " <> style) $
        levelFromStyle style `shouldBe` level
