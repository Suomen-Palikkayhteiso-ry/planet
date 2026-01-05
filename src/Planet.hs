{-# LANGUAGE OverloadedStrings #-}

module Planet (main) where

import Control.Concurrent.Async (mapConcurrently)
import qualified Data.ByteString.Lazy as LBS
import Data.List (sortOn)
import Data.Ord (Down (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Time (getCurrentTime)
import Data.Time.Zones (timeZoneForUTCTime)
import Data.Time.Zones.All (fromTZName, tzByLabel)
import System.Directory (createDirectoryIfMissing)

import Config
import ElmGen
import FeedParser
import HtmlGen
import I18n

main :: IO ()
main = do
    configContent <- TIO.readFile "planet.toml"
    case parseConfig configContent of
        Left err -> TIO.putStrLn $ "Error parsing configuration: " <> err
        Right config -> do
            putStrLn $ "Generating planet for: " ++ T.unpack (configTitle config)
            putStrLn $ "Locale: " ++ show (configLocale config)
            putStrLn $ "Found " ++ show (length $ configFeeds config) ++ " feeds."

            -- Fetch feeds concurrently
            allItems <- concat <$> mapConcurrently fetchFeed (configFeeds config)

            -- Sort by date descending
            let sortedItems = sortOn (Down . itemDate) allItems

            now <- getCurrentTime
            let messages = getMessages (configLocale config)

            -- Resolve TimeZone
            let tzName = TE.encodeUtf8 (configTimezone config)
            localTZ <- case fromTZName tzName of
                Just label -> return $ timeZoneForUTCTime (tzByLabel label) now
                Nothing -> do
                    putStrLn $ "Warning: Unknown timezone '" ++ T.unpack (configTimezone config) ++ "', falling back to UTC."
                    case fromTZName "UTC" of
                        Just utcLabel -> return $ timeZoneForUTCTime (tzByLabel utcLabel) now
                        Nothing -> error "UTC timezone not found"

            let html = generateHtml config messages sortedItems now localTZ
            let elmModule = generateElmModule sortedItems

            createDirectoryIfMissing True "public"
            LBS.writeFile "public/index.html" html
            putStrLn "Site generated in public/index.html"

            createDirectoryIfMissing True "elm-app/src"
            TIO.writeFile "elm-app/src/Data.elm" elmModule
            putStrLn "Elm data module generated in elm-app/src/Data.elm"
