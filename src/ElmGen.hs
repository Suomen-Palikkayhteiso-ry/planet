{-# LANGUAGE OverloadedStrings #-}

-- | Generate an Elm data module from Haskell data types.
-- This module generates Elm source code that embeds feed data directly,
-- avoiding the need for JSON interchange (which would redistribute data).
module ElmGen (generateElmModule) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import I18n (AppItem (..), FeedType (..))

-- | Generate a complete Elm module containing type definitions and data.
generateElmModule :: [AppItem] -> Text
generateElmModule items = T.unlines
    [ "module Data exposing (allAppItems, AppItem, FeedType(..))"
    , ""
    , ""
    , "type FeedType"
    , "    = Feed"
    , "    | YouTube"
    , "    | Image"
    , ""
    , ""
    , "type alias AppItem ="
    , "    { itemTitle : String"
    , "    , itemLink : String"
    , "    , itemDate : Maybe String"
    , "    , itemDesc : Maybe String"
    , "    , itemDescText : Maybe String"
    , "    , itemThumbnail : Maybe String"
    , "    , itemSourceTitle : String"
    , "    , itemSourceLink : Maybe String"
    , "    , itemType : FeedType"
    , "    }"
    , ""
    , ""
    , "allAppItems : List AppItem"
    , "allAppItems ="
    , renderItemList items
    ]

-- | Render a list of AppItems as an Elm list literal.
renderItemList :: [AppItem] -> Text
renderItemList [] = "    []"
renderItemList items = T.unlines $
    ["    [ " <> renderItem (head items)]
    ++ map (\i -> "    , " <> renderItem i) (tail items)
    ++ ["    ]"]

-- | Render a single AppItem as an Elm record literal.
renderItem :: AppItem -> Text
renderItem item = T.concat
    [ "{ itemTitle = "
    , renderString (itemTitle item)
    , ", itemLink = "
    , renderString (itemLink item)
    , ", itemDate = "
    , renderMaybeUTCTime (itemDate item)
    , ", itemDesc = "
    , renderMaybeString (itemDesc item)
    , ", itemDescText = "
    , renderMaybeString (itemDescText item)
    , ", itemThumbnail = "
    , renderMaybeString (itemThumbnail item)
    , ", itemSourceTitle = "
    , renderString (itemSourceTitle item)
    , ", itemSourceLink = "
    , renderMaybeString (itemSourceLink item)
    , ", itemType = "
    , renderFeedType (itemType item)
    , " }"
    ]

-- | Render a Text value as an Elm string literal.
renderString :: Text -> Text
renderString t = "\"" <> escapeElmString t <> "\""

-- | Render a Maybe Text as an Elm Maybe String.
renderMaybeString :: Maybe Text -> Text
renderMaybeString Nothing = "Nothing"
renderMaybeString (Just t) = "Just " <> renderString t

-- | Render a Maybe UTCTime as an Elm Maybe String (ISO8601 format).
renderMaybeUTCTime :: Maybe UTCTime -> Text
renderMaybeUTCTime Nothing = "Nothing"
renderMaybeUTCTime (Just t) = "Just \"" <> T.pack (iso8601Show t) <> "\""

-- | Render a FeedType as its Elm constructor name.
renderFeedType :: FeedType -> Text
renderFeedType Feed = "Feed"
renderFeedType YouTube = "YouTube"
renderFeedType Image = "Image"

-- | Escape special characters in an Elm string literal.
escapeElmString :: Text -> Text
escapeElmString = T.concatMap escapeChar
  where
    escapeChar '\\' = "\\\\"
    escapeChar '"'  = "\\\""
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c    = T.singleton c
