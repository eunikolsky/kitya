#!/usr/bin/env stack
-- stack script --resolver lts-19.31 --package base,hxt,split,directory,filepath,text,css-text,time,process,optparse-applicative,hspec,neat-interpolation --ghc-options=-hide-all-packages

-- License: GNU GPL v3.0 (see `license`)

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
    let fixHTMLNewlinesInComments' html =
          fmap head . runX $ readString [withParseHTML yes] html
            >>> fixHTMLNewlinesInComments
            >>> writeDocumentToString [withOutputXHTML, withAddDefaultDTD yes, withXmlPi no]

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

      actual <- fixHTMLNewlinesInComments' html

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

      actual <- fixHTMLNewlinesInComments' html

      actual `shouldBe` expected

    it "leaves other tags as is" $ do
      let html = mkComments [trimming|
        <div class="comment">
          <div class="comment_body">foo <a href="example.org">bar</a> </div>
        </div>
        <div class="comment">
          <div class="comment_body">foo<b></b></div>
        </div>
      |]

      let expected = html

      actual <- fixHTMLNewlinesInComments' html

      actual `shouldBe` expected

    it "prepends <br/> to newlines in text in other tags" $ do
      let html = mkComments [trimming|
        <div class="comment">
          <div class="comment_body">foo
          <a href="example.org">multi
          line</a>
          bar</div>
        </div>
        <div class="comment">
          <div class="comment_body">
            <li>foo
              <b>bo
                ld</b></li> bar
          </div>
        </div>
      |]

      let expected = mkComments [trimming|
        <div class="comment">
          <div class="comment_body">foo<br/>
          <a href="example.org">multi<br/>
          line</a><br/>
          bar</div>
        </div>
        <div class="comment">
          <div class="comment_body"><br/>
            <li>foo<br/>
              <b>bo<br/>
                ld</b></li> bar<br/>
          </div>
        </div>
      |]

      actual <- fixHTMLNewlinesInComments' html

      actual `shouldBe` expected

  describe "removeCommentersProfileLinks" $ do
    let removeCommentersProfileLinks' html =
          fmap head . runX $ readString [withParseHTML yes] html
            >>> removeCommentersProfileLinks
            >>> writeDocumentToString [withOutputXHTML, withAddDefaultDTD yes, withXmlPi no]

    it "removes commenter's profile link" $ do
      -- note: this HTML is as after `removeLinksToImages` processing
      let html = mkComments [trimming|
        <div class="comment">
          <div class="comment_subject"><span>
            <a href="http://example.livejournal.com/"><b>example</b></a>
          </span></div>
        </div>
      |]

      let expected = mkComments [trimming|
        <div class="comment">
          <div class="comment_subject"><span>
            <b>example</b>
          </span></div>
        </div>
      |]

      actual <- removeCommentersProfileLinks' html

      actual `shouldBe` expected

    it "leaves Anonymous' commenter's profile w/o link as is" $ do
      -- note: this HTML is as after `removeLinksToImages` processing
      let html = mkComments [trimming|
        <div class="comment">
          <div class="comment_subject">
            Аноним
          </div>
        </div>
      |]

      let expected = html

      actual <- removeCommentersProfileLinks' html

      actual `shouldBe` expected

    it "leaves next post link" $ do
      -- note: this HTML is as after `treeizeComments` processing
      let html = mkComments [trimming|
        <div class="comment">
          <div class="comment_subject">
            <span><a href="http://example.livejournal.com/"><b>example</b></a></span>
            <a class="next_post_link" href="next.html">next</a>
          </div>
        </div>
      |]

      let expected = mkComments [trimming|
        <div class="comment">
          <div class="comment_subject">
            <span><b>example</b></span>
            <a class="next_post_link" href="next.html">next</a>
          </div>
        </div>
      |]

      actual <- removeCommentersProfileLinks' html

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
