module Main (main, tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Data.Time
import Data.Time.Zones
import Data.Time.Zones.All
import Data.XML.Types
import qualified Text.Atom.Feed as Atom
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Feed.Types
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree (TagTree (..))
import Data.Time.Format.ISO8601 (iso8601ParseM)

import Config
import qualified FeedParser
import qualified HtmlGen
import HtmlSanitizer
import I18n

-- Helper function for tests
join :: Maybe (Maybe a) -> Maybe a
join (Just (Just x)) = Just x
join _ = Nothing

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Planet Tests"
        [ invariantsTests
        , i18nTests
        , configTests
        , feedTests
        , htmlTests
        , utilityTests
        ]

invariantsTests :: TestTree
invariantsTests =
    testGroup
        "Invariants" -- Core system invariants that must never fail
        [ testCase "HTML sanitization: cleanAndTruncate short text" $ do
            let text = T.pack "<p>Short text</p>"
            cleanAndTruncate 100 text @?= T.pack "<p>Short text</p>"
        , testCase "HTML sanitization: cleanAndTruncate long text" $ do
            let text = T.pack $ "<p>" ++ replicate 300 'a' ++ "</p>"
            let result = cleanAndTruncate 160 text
            assertBool "Truncated" (T.length result < T.length text)
            assertBool "Contains ..." (T.isInfixOf (T.pack "...") result)
        , testCase "HTML sanitization: normalizeVoids" $ do
            let tags = [TagOpen (T.pack "img") [(T.pack "src", T.pack "test")]]
                normalized = normalizeVoids tags
            length normalized @?= 2 -- Should add closing tag
        , testCase "HTML sanitization: pruneTree removes empty" $ do
            let tree = [TagBranch (T.pack "div") [] []]
                pruned = pruneTree tree
            length pruned @?= 0
        , testCase "HTML sanitization: pruneTree keeps content" $ do
            let tree = [TagBranch (T.pack "p") [] [TagLeaf (TagText (T.pack "content"))]]
                pruned = pruneTree tree
            length pruned @?= 1
        , testCase "HTML sanitization: takeWithLimit exact" $ do
            let tags = [TagText (T.pack "hello")]
                result = takeWithLimit 5 [] tags
            result @?= [TagText (T.pack "hello")]
        , testCase "HTML sanitization: takeWithLimit truncate" $ do
            let tags = [TagText (T.pack "hello world")]
                result = takeWithLimit 5 [] tags
            result @?= [TagText (T.pack "hello...")]
        ]

i18nTests :: TestTree
i18nTests =
    testGroup
        "I18n Tests" -- Covers US-002, constrained by ADR-0000
        [ testCase "parseLocale en" $ parseLocale (T.pack "en") @?= En
        , testCase "parseLocale fi" $ parseLocale (T.pack "fi") @?= Fi
        , testCase "parseLocale unknown" $ parseLocale (T.pack "unknown") @?= defaultLocale
        , testCase "getMessages En" $ msgGeneratedOn (getMessages En) @?= T.pack "Generated on"
        , testCase "getMessages Fi" $ msgGeneratedOn (getMessages Fi) @?= T.pack "Koottu"
        , testCase "getTimeLocale En" $ wDays (getTimeLocale En) @?= wDays defaultTimeLocale
        , testCase "getTimeLocale Fi" $ head (months (getTimeLocale Fi)) @?= ("tammikuu", "tammi")
        ]

configTests :: TestTree
configTests =
    testGroup
        "Config Tests" -- Covers US-002, constrained by ADR-0000
        [ testCase "PlanetMain.parseConfig valid" $ do
            let toml =
                    T.unlines
                        [ T.pack "title = \"Test Planet\""
                        , T.pack "locale = \"en\""
                        , T.pack "timezone = \"Europe/Helsinki\""
                        , T.pack "[[feeds]]"
                        , T.pack "type = \"rss\""
                        , T.pack "title = \"Test Blog\""
                        , T.pack "url = \"http://example.com/rss.xml\""
                        ]
            case parseConfig toml of
                Right config -> do
                    configTitle config @?= T.pack "Test Planet"
                    configLocale config @?= En
                    configTimezone config @?= T.pack "Europe/Helsinki"
                    length (configFeeds config) @?= 1
                    let feed = head (configFeeds config)
                    feedType feed @?= Rss
                    feedTitle feed @?= T.pack "Test Blog"
                    feedUrl feed @?= T.pack "http://example.com/rss.xml"
                Left err -> assertFailure $ "Parse failed: " ++ T.unpack err
        , testCase "PlanetMain.parseConfig invalid feed type" $ do
            let toml =
                    T.unlines
                        [ T.pack "title = \"Test\""
                        , T.pack "[[feeds]]"
                        , T.pack "type = \"invalid\""
                        , T.pack "title = \"Test\""
                        , T.pack "url = \"http://example.com\""
                        ]
            case parseConfig toml of
                Left _ -> return () -- Should fail
                Right _ -> assertFailure "Should have failed"
        , testCase "PlanetMain.parseConfig missing title" $ do
            let toml =
                    T.unlines
                        [ T.pack "[[feeds]]"
                        , T.pack "type = \"rss\""
                        , T.pack "title = \"Test\""
                        , T.pack "url = \"http://example.com\""
                        ]
            case parseConfig toml of
                Right config -> configTitle config @?= T.pack "Planet"
                Left _ -> assertFailure "Should succeed with default title"
        , testCase "PlanetMain.parseConfig atom type" $ do
            let toml =
                    T.unlines
                        [ T.pack "title = \"Test\""
                        , T.pack "[[feeds]]"
                        , T.pack "type = \"atom\""
                        , T.pack "title = \"Test\""
                        , T.pack "url = \"http://example.com\""
                        ]
            case parseConfig toml of
                Right config -> do
                    let feed = head (configFeeds config)
                    feedType feed @?= Atom
                Left err -> assertFailure $ "Parse failed: " ++ T.unpack err
        ]

feedTests :: TestTree
feedTests =
    testGroup
        "Feed Tests" -- Covers US-001, US-006, constrained by ADR-0000
        [ testCase "PlanetMain.extractFirstImage" $ do
            let html = T.pack "<p>Some text <img src=\"http://example.com/image.jpg\" alt=\"test\"> more text</p>"
            FeedParser.extractFirstImage html @?= Just (T.pack "http://example.com/image.jpg")
        , testCase "PlanetMain.extractFirstImage no image" $ do
            let html = T.pack "<p>Some text without image</p>"
            FeedParser.extractFirstImage html @?= Nothing
        , testCase "PlanetMain.getMediaDescription" $ do
            let mediaElements = [Element (Name (T.pack "description") (Just (T.pack "http://search.yahoo.com/mrss/")) Nothing) [] [NodeContent (ContentText (T.pack "Test description"))]]
            FeedParser.getMediaDescriptionFromElements mediaElements @?= Just (T.pack "Test description")
        , testCase "PlanetMain.isMediaDescription true" $ do
            let e = Element (Name (T.pack "description") (Just (T.pack "http://search.yahoo.com/mrss/")) Nothing) [] []
            FeedParser.isMediaDescription e @?= True
        , testCase "PlanetMain.isMediaDescription false" $ do
            let e = Element (Name (T.pack "description") Nothing Nothing) [] []
            FeedParser.isMediaDescription e @?= False
        , testCase "PlanetMain.isMediaThumbnail true" $ do
            let e = Element (Name (T.pack "thumbnail") (Just (T.pack "http://search.yahoo.com/mrss/")) Nothing) [] []
            FeedParser.isMediaThumbnail e @?= True
        , testCase "PlanetMain.isMediaGroup true" $ do
            let e = Element (Name (T.pack "group") (Just (T.pack "http://search.yahoo.com/mrss/")) Nothing) [] []
            FeedParser.isMediaGroup e @?= True
        , testCase "PlanetMain.getUrlAttr" $ do
            let attrs = [(Name (T.pack "url") Nothing Nothing, [ContentText (T.pack "http://example.com")])]
                e = Element (Name (T.pack "thumbnail") Nothing Nothing) attrs []
            FeedParser.getUrlAttr e @?= Just (T.pack "http://example.com")
        , testCase "PlanetMain.getUrlAttr no url" $ do
            let attrs = [(Name (T.pack "other") Nothing Nothing, [ContentText (T.pack "value")])]
                e = Element (Name (T.pack "thumbnail") Nothing Nothing) attrs []
            FeedParser.getUrlAttr e @?= Nothing
        , testCase "PlanetMain.findMediaThumbnail" $ do
            let e = Element (Name (T.pack "thumbnail") (Just (T.pack "http://search.yahoo.com/mrss/")) Nothing) [(Name (T.pack "url") Nothing Nothing, [ContentText (T.pack "http://example.com")])] []
                mediaElements = [e]
            FeedParser.findMediaThumbnail mediaElements @?= Just (T.pack "http://example.com")
        , testCase "PlanetMain.findMediaGroupThumbnail" $ do
            let thumb = Element (Name (T.pack "thumbnail") (Just (T.pack "http://search.yahoo.com/mrss/")) Nothing) [(Name (T.pack "url") Nothing Nothing, [ContentText (T.pack "http://example.com")])] []
                group = Element (Name (T.pack "group") (Just (T.pack "http://search.yahoo.com/mrss/")) Nothing) [] [NodeElement thumb]
                mediaElements = [group]
            FeedParser.findMediaGroupThumbnail mediaElements @?= Just (T.pack "http://example.com")
        , testCase "join Just Just" $ join (Just (Just "test")) @?= Just "test"
        , testCase "join Just Nothing" $ (join (Just Nothing) :: Maybe String) @?= Nothing
        , testCase "join Nothing" $ (join Nothing :: Maybe String) @?= Nothing
        , testCase "FeedParser.stripFirstPTag with p tag" $ do
            -- Covers US-006
            let html = T.pack "<p>This is content</p><p>More</p>"
            FeedParser.stripFirstPTag html @?= T.pack "<p>More</p>"
        , testCase "FeedParser.stripFirstPTag flickr-like description" $ do
            let html = T.pack "<p><a href=\"https://www.flickr.com/photos/12345/67890/\">Photo on Flickr</a></p>This is the actual description. It has <b>some</b> formatting."
            FeedParser.stripFirstPTag html @?= T.pack "This is the actual description. It has <b>some</b> formatting."
        , testCase "FeedParser.stripFirstPTag flickr encoded content" $ do
            let html = T.pack "&lt;p&gt;&lt;a href=&quot;https://www.flickr.com/people/infamousq/&quot;&gt;InfamousQ&lt;/a&gt; posted a photo:&lt;/p&gt;\n\t\n&lt;p&gt;&lt;a href=&quot;https://www.flickr.com/photos/infamousq/54774659725/&quot; title=&quot;Plan for Tervahovi LEGO display version 2&quot;&gt;&lt;img src=&quot;https://live.staticflickr.com/65535/54774659725_f267ce07b2_m.jpg&quot; width=&quot;240&quot; height=&quot;135&quot; alt=&quot;Plan for Tervahovi LEGO display version 2&quot; /&gt;&lt;/a&gt;&lt;/p&gt;\n\n&lt;p&gt;Further development of the Tervahovi harbor area&lt;/p&gt;"
            FeedParser.stripFirstPTag html @?= T.pack "\n\t\n<p><a href=\"https://www.flickr.com/photos/infamousq/54774659725/\" title=\"Plan for Tervahovi LEGO display version 2\"><img src=\"https://live.staticflickr.com/65535/54774659725_f267ce07b2_m.jpg\" width=\"240\" height=\"135\" alt=\"Plan for Tervahovi LEGO display version 2\"></img></a></p>\n\n<p>Further development of the Tervahovi harbor area</p>"
        , testCase "FeedParser.cleanTitle remove hashtags" $ do
            -- Covers US-001
            let title = T.pack "My post #tag1 #tag2"
            FeedParser.cleanTitle title @?= T.pack "My post"
        , testCase "FeedParser.cleanTitle keep number hashtags" $ do
            -- Covers US-001
            let title = T.pack "My post #123 #tag"
            FeedParser.cleanTitle title @?= T.pack "My post #123"
        , testCase "FeedParser.cleanTitle no hashtags" $ do
            -- Covers US-001
            let title = T.pack "My post without hashtags"
            FeedParser.cleanTitle title @?= T.pack "My post without hashtags"
        , testCase "getFlickrMediaDescription with HTMLContent" $ do
            let now = read "2023-01-01 00:00:00 UTC"
                entry =
                    (Atom.nullEntry (T.pack "tag:example.com,2023:test") (undefined :: Atom.TextContent) now)
                        { Atom.entryContent = Just (Atom.HTMLContent (T.pack "<p>first</p><p>second</p>"))
                        }
                item = AtomItem entry
            FeedParser.getFlickrMediaDescription item @?= Just (T.pack "<p>second</p>")
        , testCase "getAtomMediaDescription with HTMLContent" $ do
            let now = read "2023-01-01 00:00:00 UTC"
                entry =
                    (Atom.nullEntry (T.pack "tag:example.com,2023:test") (undefined :: Atom.TextContent) now)
                        { Atom.entryContent = Just (Atom.HTMLContent (T.pack "<p>first</p><p>second</p>"))
                        }
                item = AtomItem entry
            FeedParser.getAtomMediaDescription item @?= Just (T.pack "<p>first</p><p>second</p>")
        , testCase "FeedParser.parseItem Atom published date preferred" $ do
            let publishedDate = T.pack "2025-12-31T23:55:00Z"
                updatedDate = T.pack "2026-01-01T17:45:09Z"
                entry = (Atom.nullEntry (T.pack "id") (Atom.TextString (T.pack "Test Atom Title")) (read "2000-01-01 00:00:00 UTC"))
                    { Atom.entryPublished = Just publishedDate
                    , Atom.entryUpdated = updatedDate
                    , Atom.entryLinks = [Atom.nullLink (T.pack "http://example.com/atom-link")]
                    }
                item = AtomItem entry
                feedConfig = FeedConfig Atom (T.pack "Test Feed") (T.pack "http://example.com")
                expectedDate = iso8601ParseM (T.unpack publishedDate)
            
            FeedParser.parseItem feedConfig Nothing item @?= Just (AppItem (T.pack "Test Atom Title") (T.pack "http://example.com/atom-link") expectedDate Nothing Nothing (T.pack "Test Feed") Nothing Atom)

        , testCase "FeedParser.parseItem Atom updated date used if published missing" $ do
            let updatedDate = T.pack "2026-01-01T17:45:09Z"
                entry = (Atom.nullEntry (T.pack "id") (Atom.TextString (T.pack "Test Atom Title")) (read "2000-01-01 00:00:00 UTC"))
                    { Atom.entryPublished = Nothing
                    , Atom.entryUpdated = updatedDate
                    , Atom.entryLinks = [Atom.nullLink (T.pack "http://example.com/atom-link")]
                    }
                item = AtomItem entry
                feedConfig = FeedConfig Atom (T.pack "Test Feed") (T.pack "http://example.com")
                expectedDate = iso8601ParseM (T.unpack updatedDate)

            FeedParser.parseItem feedConfig Nothing item @?= Just (AppItem (T.pack "Test Atom Title") (T.pack "http://example.com/atom-link") expectedDate Nothing Nothing (T.pack "Test Feed") Nothing Atom)
        ]

htmlTests :: TestTree
htmlTests =
    testGroup
        "HTML Tests" -- Covers US-005, constrained by ADR-0000
        [ testCase "PlanetMain.generateHtml basic" $ do
            let config = Config (T.pack "Test Planet") [] En (T.pack "Europe/Helsinki")
                msgs = getMessages En
                items = []
                now = read "2023-01-01 00:00:00 UTC"
                tz = timeZoneForUTCTime (tzByLabel Europe__Helsinki) now
                html = HtmlGen.generateHtml config msgs items now tz
            -- Just check it doesn't crash and contains expected content
            let htmlText = TE.decodeUtf8 $ LBS.toStrict html
            assertBool "Contains title" (T.isInfixOf (T.pack "Test Planet") htmlText)
        , testCase "PlanetMain.renderCard with date" $ do
            let item = AppItem (T.pack "Test Title") (T.pack "http://example.com") (Just $ read "2023-01-01 00:00:00 UTC") (Just (T.pack "Test desc")) Nothing (T.pack "Test Source") Nothing Rss
                locale = defaultTimeLocale
                card = HtmlGen.renderCard locale item
                rendered = renderHtml card
            assertBool "Contains date" (TL.isInfixOf (TL.pack "2023-01-01") rendered)
        ]

utilityTests :: TestTree
utilityTests =
    testGroup
        "Utility Tests" -- Covers US-005, constrained by ADR-0000
        [ testCase "join Just Just" $ join (Just (Just "x")) @?= Just "x"
        , testCase "join Just Nothing" $ join (Just Nothing :: Maybe (Maybe String)) @?= Nothing
        , testCase "join Nothing" $ join (Nothing :: Maybe (Maybe String)) @?= Nothing
        ]
