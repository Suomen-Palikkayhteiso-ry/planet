{-# LANGUAGE OverloadedStrings #-}

module Planet (main) where

import Control.Concurrent.Async (mapConcurrently)
import qualified Data.ByteString.Lazy as LBS
import Data.List (sortOn)
import Data.Ord (Down (..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)

import Config
import ElmGen
import FeedParser
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

            let elmModule = generateElmModule sortedItems

            -- No longer creating public/data.json
            createDirectoryIfMissing True "elm-app/src"
            TIO.writeFile "elm-app/src/Data.elm" elmModule
            putStrLn "Elm data module generated in elm-app/src/Data.elm"
