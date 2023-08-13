#!/usr/bin/env stack
-- stack script --resolver lts-19.31 --package base,hxt,split,directory,filepath,text,css-text,time,process,hspec,neat-interpolation --ghc-options=-hide-all-packages

{-# LANGUAGE ImportQualifiedPost, QuasiQuotes #-}

module Test where

import Control.Monad
import Data.Text qualified as T
import Kitya hiding (main)
import NeatInterpolation
import Test.Hspec
import Text.XML.HXT.Core

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "levelFromStyle" $
    forM_ [("margin-left: 00px;", 0), ("margin-left: 120px;", 4)] $ \(style, level) ->
      it ("parses level from style " <> style) $
        levelFromStyle style `shouldBe` level

  describe "removeBodyInComments" $
    it "removes wrapping body tag in comments" $ do
      let html = T.unpack [trimming|
        <html>
        <body>body
          <div id="comm" class="comments">
            <div class="comment">
              <div class="comment_body">
                <body>foo bar</body>
              </div>
            </div>
            <div class="comment">
              <div class="comment_body">
                <body>второй</body>
              </div>
            </div>
          </div>
        </body>
        </html>
      |]

      let expected = T.unpack [trimming|
        <html>
        <body>body
          <div id="comm" class="comments">
            <div class="comment">
              <div class="comment_body">
                foo bar
              </div>
            </div>
            <div class="comment">
              <div class="comment_body">
                второй
              </div>
            </div>
          </div>
        </body>
        </html>
      |]

      actual <- fmap head . runX $ readString [withParseHTML yes] html
        >>> removeBodyInComments
        >>> writeDocumentToString [withOutputXHTML, withAddDefaultDTD yes, withXmlPi no]

      actual `shouldBe` expected

  describe "fixHTMLNewlinesInComments" $ do
    it "leaves text comments w/o newlines as is" $ do
      let html = mkComments [trimming|
        <div class="comment">
          <div class="comment_body">foo bar</div>
        </div>
        <div class="comment">
          <div class="comment_body">второй</div>
        </div>
      |]

      let expected = html

      actual <- fmap head . runX $ readString [withParseHTML yes] html
        >>> fixHTMLNewlinesInComments
        >>> writeDocumentToString [withOutputXHTML, withAddDefaultDTD yes, withXmlPi no]

      actual `shouldBe` expected

    it "prepends <br/> to newlines in text comments" $ do
      let html = mkComments [trimming|
        <div class="comment">
          <div class="comment_body">foo

          bar</div>
        </div>
        <div class="comment">
          <div class="comment_body">
            второй
              комментарий
            </div>
        </div>
      |]

      let expected = mkComments [trimming|
        <div class="comment">
          <div class="comment_body">foo<br/>
        <br/>
          bar</div>
        </div>
        <div class="comment">
          <div class="comment_body"><br/>
            второй<br/>
              комментарий<br/>
            </div>
        </div>
      |]

      actual <- fmap head . runX $ readString [withParseHTML yes] html
        >>> fixHTMLNewlinesInComments
        >>> writeDocumentToString [withOutputXHTML, withAddDefaultDTD yes, withXmlPi no]

      actual `shouldBe` expected

mkComments :: T.Text -> String
mkComments commentsText = T.unpack [trimming|
  <html>
  <body>body
    <div id="comm" class="comments">
      $commentsText
    </div>
  </body>
  </html>
|]
