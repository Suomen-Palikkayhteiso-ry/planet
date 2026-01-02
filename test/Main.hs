module TestMain where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.Format (TimeLocale(..), defaultTimeLocale)
import Data.Time.Zones
import Data.Time.Zones.All
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Text.Feed.Types
import qualified Text.Atom.Feed as Atom
import qualified Text.RSS.Syntax as RSS
import Data.XML.Types
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.HTML.TagSoup

import Main
import I18n

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Planet Tests"
  [ configTests
  ]

i18nTests :: TestTree
i18nTests = testGroup "I18n Tests"
  [ testCase "parseLocale en" $ parseLocale "en" @?= En
  , testCase "parseLocale fi" $ parseLocale "fi" @?= Fi
  , testCase "parseLocale unknown" $ parseLocale "unknown" @?= defaultLocale
  , testCase "getMessages En" $ msgGeneratedOn (getMessages En) @?= "Generated on"
  , testCase "getMessages Fi" $ msgGeneratedOn (getMessages Fi) @?= "Koottu"
  , testCase "getTimeLocale En" $ wDays (getTimeLocale En) @?= wDays defaultTimeLocale
  , testCase "getTimeLocale Fi" $ months (getTimeLocale Fi) !! 0 @?= ("tammikuu", "tammi")
  ]

configTests :: TestTree
configTests = testGroup "Config Tests"
  [ testCase "parseConfig valid" $ do
      let toml = T.unlines
            [ "title = \"Test Planet\""
            , "locale = \"en\""
            , "timezone = \"Europe/Helsinki\""
            , "[[feeds]]"
            , "type = \"blog\""
            , "title = \"Test Blog\""
            , "url = \"http://example.com/rss.xml\""
            ]
      case parseConfig toml of
        Right config -> do
          configTitle config @?= "Test Planet"
          configLocale config @?= En
          configTimezone config @?= "Europe/Helsinki"
          length (configFeeds config) @?= 1
          let feed = head (configFeeds config)
          feedType feed @?= Blog
          feedTitle feed @?= "Test Blog"
          feedUrl feed @?= "http://example.com/rss.xml"
        Left err -> assertFailure $ "Parse failed: " ++ T.unpack err
  , testCase "parseConfig invalid feed type" $ do
      let toml = T.unlines
            [ "title = \"Test\""
            , "[[feeds]]"
            , "type = \"invalid\""
            , "title = \"Test\""
            , "url = \"http://example.com\""
            ]
      case parseConfig toml of
        Left _ -> return () -- Should fail
        Right _ -> assertFailure "Should have failed"
  , testCase "parseConfig missing title" $ do
      let toml = T.unlines
            [ "[[feeds]]"
            , "type = \"blog\""
            , "title = \"Test\""
            , "url = \"http://example.com\""
            ]
      case parseConfig toml of
        Right config -> configTitle config @?= "Planet"
        Left _ -> assertFailure "Should succeed with default title"
  ]

feedTests :: TestTree
feedTests = testGroup "Feed Tests"
  [ testCase "extractFirstImage" $ do
      let html = "<p>Some text <img src=\"http://example.com/image.jpg\" alt=\"test\"> more text</p>"
      extractFirstImage html @?= Just "http://example.com/image.jpg"
  , testCase "extractFirstImage no image" $ do
      let html = "<p>Some text without image</p>"
      extractFirstImage html @?= Nothing
  , testCase "getMediaDescription" $ do
      let elements = [Element (Name "description" (Just "http://search.yahoo.com/mrss/") Nothing) [] [NodeContent (ContentText "Test description")]]
      getMediaDescription elements @?= Just "Test description"
  , testCase "isMediaDescription true" $ do
      let e = Element (Name "description" (Just "http://search.yahoo.com/mrss/") Nothing) [] []
      isMediaDescription e @?= True
  , testCase "isMediaDescription false" $ do
      let e = Element (Name "description" Nothing Nothing) [] []
      isMediaDescription e @?= False
  , testCase "isMediaThumbnail true" $ do
      let e = Element (Name "thumbnail" (Just "http://search.yahoo.com/mrss/") Nothing) [] []
      isMediaThumbnail e @?= True
  , testCase "isMediaGroup true" $ do
      let e = Element (Name "group" (Just "http://search.yahoo.com/mrss/") Nothing) [] []
      isMediaGroup e @?= True
  , testCase "getUrlAttr" $ do
      let attrs = [(Name "url" Nothing Nothing, [ContentText "http://example.com"])]
          e = Element (Name "thumbnail" Nothing Nothing) attrs []
      getUrlAttr e @?= Just "http://example.com"
  , testCase "getUrlAttr no url" $ do
      let attrs = [(Name "other" Nothing Nothing, [ContentText "value"])]
          e = Element (Name "thumbnail" Nothing Nothing) attrs []
      getUrlAttr e @?= Nothing
  , testCase "findMediaThumbnail" $ do
      let e = Element (Name "thumbnail" (Just "http://search.yahoo.com/mrss/") Nothing) [(Name "url" Nothing Nothing, [ContentText "http://example.com"])] []
          elements = [e]
      findMediaThumbnail elements @?= Just "http://example.com"
  , testCase "findMediaGroupThumbnail" $ do
      let thumb = Element (Name "thumbnail" (Just "http://search.yahoo.com/mrss/") Nothing) [(Name "url" Nothing Nothing, [ContentText "http://example.com"])] []
          group = Element (Name "group" (Just "http://search.yahoo.com/mrss/") Nothing) [] [NodeElement thumb]
          elements = [group]
      findMediaGroupThumbnail elements @?= Just "http://example.com"
  , testCase "join Just Just" $ join (Just (Just "test")) @?= Just "test"
  , testCase "join Just Nothing" $ join (Just Nothing) @?= Nothing
  , testCase "join Nothing" $ join Nothing @?= Nothing
  ]

htmlTests :: TestTree
htmlTests = testGroup "HTML Tests"
  [ testCase "generateHtml basic" $ do
      let config = Config "Test Planet" [] En "Europe/Helsinki"
          msgs = getMessages En
          items = []
          now = read "2023-01-01 00:00:00 UTC"
          tz = timeZoneForUTCTime (tzByLabel Europe__Helsinki) now
          html = generateHtml config msgs items now tz
      -- Just check it doesn't crash and contains expected content
      let rendered = renderHtml html
      assertBool "Contains title" (T.isInfixOf "Test Planet" rendered)
  , testCase "renderCard with date" $ do
      let item = AppItem "Test Title" "http://example.com" (Just $ read "2023-01-01 00:00:00 UTC") (Just "Test desc") Nothing "Test Source" Blog
          locale = defaultTimeLocale
          card = renderCard locale item
          rendered = renderHtml card
      assertBool "Contains date" (T.isInfixOf "2023-01-01" rendered)
  , testCase "itemMonth with date" $ do
      let item = AppItem "Test" "http://example.com" (Just $ read "2023-01-01 00:00:00 UTC") Nothing Nothing "Test" Blog
          locale = defaultTimeLocale
      itemMonth item @?= "2023-01"
  , testCase "itemMonth without date" $ do
      let item = AppItem "Test" "http://example.com" Nothing Nothing Nothing "Test" Blog
      itemMonth item @?= "Unknown"
  , testCase "mkGroup" $ do
      let items = [AppItem "Test1" "http://example.com" (Just $ read "2023-01-01 00:00:00 UTC") Nothing Nothing "Test" Blog,
                   AppItem "Test2" "http://example.com" (Just $ read "2023-01-02 00:00:00 UTC") Nothing Nothing "Test" Blog]
          locale = defaultTimeLocale
          (label, id, groupItems) = mkGroup items
      label @?= "January 2023"
      length groupItems @?= 2
  ]

utilityTests :: TestTree
utilityTests = testGroup "Utility Tests"
  [ testCase "cleanAndTruncate short text" $ do
      let text = "<p>Short text</p>"
      cleanAndTruncate 100 text @?= "<p>Short text</p>"
  , testCase "cleanAndTruncate long text" $ do
      let text = "<p>" <> T.replicate 200 "a" <> "</p>"
      let result = cleanAndTruncate 50 text
      assertBool "Truncated" (T.length result < 100)
      assertBool "Ends with ..." (T.isSuffixOf "..." result)
  , testCase "normalizeVoids" $ do
      let tags = [TagOpen "img" [("src", "test")]]
          normalized = normalizeVoids tags
      length normalized @?= 2 -- Should add closing tag
  , testCase "pruneTree removes empty" $ do
      let tree = [TagBranch "div" [] []]
          pruned = pruneTree tree
      length pruned @?= 0
  , testCase "pruneTree keeps content" $ do
      let tree = [TagBranch "p" [] [TagLeaf (TagText "content")]]
          pruned = pruneTree tree
      length pruned @?= 1
  , testCase "takeWithLimit exact" $ do
      let tags = [TagText "hello"]
          result = takeWithLimit 5 [] tags
      result @?= [TagText "hello"]
  , testCase "takeWithLimit truncate" $ do
      let tags = [TagText "hello world"]
          result = takeWithLimit 5 [] tags
      result @?= [TagText "hello..."]
  , testCase "showItemType Atom" $ showItemType (AtomItem undefined) @?= "Atom"
  , testCase "showItemType RSS" $ showItemType (RSSItem undefined) @?= "RSS"
  , testCase "showItemType XML" $ showItemType (XMLItem undefined) @?= "XML"
  ]