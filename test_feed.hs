{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Simple (httpLBS, getResponseBody, parseRequest)
import qualified Data.ByteString.Lazy as LBS
import qualified Text.Atom.Feed as Atom
import Text.Feed.Import (parseFeedSource)
import Text.Feed.Types (Feed(..))
import Data.XML.Types (Element(..), Name(..), Node(..), Content(..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    let url = "https://www.youtube.com/feeds/videos.xml?channel_id=UC9-y-6csu5WGm29I7JiwpnA" -- Computerphile
    req <- parseRequest url
    response <- httpLBS req
    let content = getResponseBody response
    
    case parseFeedSource content of
        Just (AtomFeed feed) -> do
            let entries = Atom.feedEntries feed
            mapM_ inspectEntry (take 2 entries)
        Just _ -> putStrLn "Parsed as non-Atom feed"
        Nothing -> putStrLn "Failed to parse feed"

inspectEntry :: Atom.Entry -> IO ()
inspectEntry entry = do
    TIO.putStrLn $ "Title: " <> Atom.txtToString (Atom.entryTitle entry)
    let others = Atom.entryOther entry
    TIO.putStrLn $ "Other elements count: " <> (T.pack $ show $ length others)
    mapM_ printElement others

printElement :: Element -> IO ()
printElement e = do
    TIO.putStrLn $ "Element: " <> (T.pack $ show $ elementName e)
    TIO.putStrLn $ "Attrs: " <> (T.pack $ show $ elementAttributes e)
    TIO.putStrLn "---"
