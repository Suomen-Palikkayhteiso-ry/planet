module ConfigSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Text (Text)
import qualified Data.Text as T

import Config

configTests :: TestTree
configTests =
    testGroup
        "Config Tests"
        [ testCase "parseConfig valid" $ do
            let toml =
                    T.unlines
                        [ T.pack "title = \"Test Planet\""
                        , T.pack "locale = \"en\""
                        , T.pack "timezone = \"Europe/Helsinki\""
                        , T.pack "[[feeds]]"
                        , T.pack "type = \"blog\""
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
                    feedType feed @?= Blog
                    feedTitle feed @?= T.pack "Test Blog"
                    feedUrl feed @?= T.pack "http://example.com/rss.xml"
                Left err -> assertFailure $ "Parse failed: " ++ T.unpack err
        , testCase "parseConfig invalid feed type" $ do
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
        , testCase "parseConfig missing title" $ do
            let toml =
                    T.unlines
                        [ T.pack "[[feeds]]"
                        , T.pack "type = \"blog\""
                        , T.pack "title = \"Test\""
                        , T.pack "url = \"http://example.com\""
                        ]
            case parseConfig toml of
                Right config -> configTitle config @?= T.pack "Planet"
                Left _ -> assertFailure "Should succeed with default title"
        ]
