{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module FeedParser (
    FeedHandler (..),
    getFeedHandler,
    fetchFeed,
    parseItem,
    getMediaDescription,
    getYouTubeMediaDescription,
    getFlickrMediaDescription,
    stripFirstPTag,
    cleanTitle,
    getMediaDescriptionFromElements,
    isMediaDescription,
    isMediaGroup,
    extractFirstImage,
    getItemThumbnail,
    elementChildren,
    getAtomThumbnail,
    findEnclosureImage,
    getRSSThumbnail,
    findMediaThumbnail,
    findMediaGroupThumbnail,
    isMediaThumbnail,
    getUrlAttr,
    join,
) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isDigit)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.XML.Types (Content (..), Element (..), Name (..), Node (..))
import Network.HTTP.Simple (Response, getResponseBody, httpLBS, parseRequest)
import qualified Text.Atom.Feed as Atom
import Text.Feed.Import (parseFeedSource)
import Text.Feed.Query (getFeedItems, getItemDescription, getItemLink, getItemPublishDate, getItemTitle)
import Text.Feed.Types (Feed (..), Item (..))
import Text.HTML.TagSoup (Tag (..), parseTags, renderTags)
import qualified Text.RSS.Syntax as RSS

import Config
import I18n

newtype FeedHandler = FeedHandler
    { fhGetMediaDescription :: Item -> Maybe Text
    }

getFeedHandler :: FeedType -> FeedHandler
getFeedHandler YouTube = FeedHandler{fhGetMediaDescription = getYouTubeMediaDescription}
getFeedHandler Flickr = FeedHandler{fhGetMediaDescription = getFlickrMediaDescription}
getFeedHandler Blog = FeedHandler{fhGetMediaDescription = const Nothing}

-- Fetching
fetchFeed :: FeedConfig -> IO [AppItem]
fetchFeed fc = do
    putStrLn $ "Fetching " ++ T.unpack (feedTitle fc) ++ "..."
    req <- parseRequest (T.unpack $ feedUrl fc)
    result <- try (httpLBS req) :: IO (Either SomeException (Response LBS.ByteString))
    case result of
        Left err -> do
            putStrLn $ "Error fetching " ++ T.unpack (feedTitle fc) ++ ": " ++ show err
            return []
        Right response -> do
            let content = getResponseBody response
            case parseFeedSource content of
                Nothing -> do
                    putStrLn $ "Failed to parse feed: " ++ T.unpack (feedTitle fc)
                    return []
                Just feed -> let altLink = getFeedAlternateLink feed in return $ mapMaybe (parseItem fc altLink) (getFeedItems feed)

-- Helper for date join
join :: Maybe (Maybe a) -> Maybe a
join (Just (Just x)) = Just x
join _ = Nothing

-- Base parseItem function
parseItem :: FeedConfig -> Maybe Text -> Item -> Maybe AppItem
parseItem fc altLink item = do
    rawTitle <- getItemTitle item
    let title = cleanTitle rawTitle
    link <- getItemLink item
    let date = join $ getItemPublishDate item
    let defaultDesc = getItemDescription item
    let mediaDesc = getMediaDescription fc item
    let desc = mediaDesc <|> defaultDesc
    let thumb = getItemThumbnail item <|> (desc >>= extractFirstImage)
    return $ AppItem title link date desc thumb (feedTitle fc) altLink (feedType fc)

-- Media Description Extraction (feed-type specific)
getMediaDescription :: FeedConfig -> Item -> Maybe Text
getMediaDescription fc = fhGetMediaDescription (getFeedHandler (feedType fc))

getYouTubeMediaDescription :: Item -> Maybe Text
getYouTubeMediaDescription item = case item of
    AtomItem entry -> getMediaDescriptionFromElements (Atom.entryOther entry)
    XMLItem element -> getMediaDescriptionFromElements (elementChildren element)
    _ -> Nothing

getFlickrMediaDescription :: Item -> Maybe Text
getFlickrMediaDescription item = case item of
    AtomItem entry ->
        case Atom.entryContent entry of
            Just (Atom.HTMLContent content) -> Just $ stripFirstPTag content
            _ -> fmap stripFirstPTag (getItemDescription item) -- fallback
    XMLItem element -> getMediaDescriptionFromElements (elementChildren element)
    _ -> Nothing

stripFirstPTag :: Text -> Text
stripFirstPTag html =
    let decodedHtml = decodeEntities html
        tags = parseTags decodedHtml
        remainingTags = skipToFirstPTag tags
     in renderTags remainingTags
  where
    decodeEntities :: Text -> Text
    decodeEntities = T.replace "&lt;" "<" . T.replace "&gt;" ">" . T.replace "&quot;" "\"" . T.replace "&amp;" "&"
    skipToFirstPTag :: [Tag Text] -> [Tag Text]
    skipToFirstPTag [] = []
    skipToFirstPTag (TagOpen "p" _ : rest) =
        let (_, remaining) = skipUntilClosingPTag rest
         in remaining
    skipToFirstPTag (_ : rest) = skipToFirstPTag rest
    skipUntilClosingPTag :: [Tag Text] -> ([Tag Text], [Tag Text])
    skipUntilClosingPTag [] = ([], [])
    skipUntilClosingPTag (TagClose "p" : rest) = ([], rest)
    skipUntilClosingPTag (tag : rest) =
        let (skipped, remaining) = skipUntilClosingPTag rest
         in (tag : skipped, remaining)

cleanTitle :: Text -> Text
cleanTitle title =
    let wordList = T.words title
        cleaned = reverse $ dropWhile isHashtagToRemove $ reverse wordList
     in T.unwords cleaned
  where
    isHashtagToRemove w = T.isPrefixOf "#" w && not (T.all isDigit (T.drop 1 w))

getMediaDescriptionFromElements :: [Element] -> Maybe Text
getMediaDescriptionFromElements elements =
    -- First check for descriptions directly in the elements
    case filter isMediaDescription elements of
        (d : _) -> elementText d
        [] ->
            -- Then check inside media groups
            let mediaGroups = filter isMediaGroup elements
             in case mediaGroups of
                    (g : _) ->
                        let descriptionElements = filter isMediaDescription (elementNodes g >>= nodeToElem)
                         in case descriptionElements of
                                (d : _) -> elementText d
                                _ -> Nothing
                    _ -> Nothing
  where
    nodeToElem (NodeElement e) = [e]
    nodeToElem _ = []

    elementText e =
        let texts = [t | NodeContent (ContentText t) <- elementNodes e]
         in if null texts then Nothing else Just (T.concat texts)

isMediaDescription :: Element -> Bool
isMediaDescription e =
    nameLocalName (elementName e) == "description"
        && nameNamespace (elementName e) == Just "http://search.yahoo.com/mrss/"

isMediaGroup :: Element -> Bool
isMediaGroup e =
    nameLocalName (elementName e) == "group"
        && nameNamespace (elementName e) == Just "http://search.yahoo.com/mrss/"

extractFirstImage :: Text -> Maybe Text
extractFirstImage html =
    let tags = parseTags html
        imgTags = filter (\case TagOpen "img" _ -> True; _ -> False) tags
     in case imgTags of
            (TagOpen "img" attrs : _) -> lookup "src" attrs
            _ -> Nothing

-- Thumbnail extraction
getItemThumbnail :: Item -> Maybe Text
getItemThumbnail item = case item of
    AtomItem entry -> getAtomThumbnail entry
    RSSItem rssItem -> getRSSThumbnail rssItem
    XMLItem element -> findMediaThumbnail (elementChildren element) <|> findMediaGroupThumbnail (elementChildren element)
    _ -> Nothing

elementChildren :: Element -> [Element]
elementChildren e = [c | NodeElement c <- elementNodes e]

getAtomThumbnail :: Atom.Entry -> Maybe Text
getAtomThumbnail entry =
    let others = Atom.entryOther entry
        enclosureImg = findEnclosureImage (Atom.entryLinks entry)
     in case findMediaThumbnail others of
            Just url -> Just url
            Nothing -> findMediaGroupThumbnail others <|> enclosureImg

findEnclosureImage :: [Atom.Link] -> Maybe Text
findEnclosureImage links =
    case filter (\l -> Atom.linkRel l == Just (Right (T.pack "enclosure")) && maybe False (T.isPrefixOf "image/") (Atom.linkType l)) links of
        (l : _) -> Just (Atom.linkHref l)
        [] -> Nothing

getRSSThumbnail :: RSS.RSSItem -> Maybe Text
getRSSThumbnail item =
    case findMediaThumbnail (RSS.rssItemOther item) of
        Just url -> Just url
        Nothing -> findMediaGroupThumbnail (RSS.rssItemOther item)

findMediaThumbnail :: [Element] -> Maybe Text
findMediaThumbnail elems =
    case filter isMediaThumbnail elems of
        (e : _) -> getUrlAttr e
        [] -> Nothing

findMediaGroupThumbnail :: [Element] -> Maybe Text
findMediaGroupThumbnail elems =
    case filter isMediaGroup elems of
        (g : _) -> findMediaThumbnail (elementNodes g >>= nodeToElem)
        [] -> Nothing
  where
    nodeToElem (NodeElement e) = [e]
    nodeToElem _ = []

isMediaThumbnail :: Element -> Bool
isMediaThumbnail e =
    nameLocalName (elementName e) == "thumbnail"
        && nameNamespace (elementName e) == Just "http://search.yahoo.com/mrss/"

getUrlAttr :: Element -> Maybe Text
getUrlAttr e =
    case filter (\(n, _) -> nameLocalName n == "url") (elementAttributes e) of
        ((_, [ContentText t]) : _) -> Just t
        _ -> Nothing

getFeedAlternateLink :: Text.Feed.Types.Feed -> Maybe Text
getFeedAlternateLink feed = case feed of
    Text.Feed.Types.AtomFeed af -> getAtomFeedAlternateLink af
    Text.Feed.Types.RSSFeed rf -> getRSSFeedAlternateLink rf
    _ -> Nothing
  where
    getAtomFeedAlternateLink af = 
        case Atom.feedLinks af of
            (l:_) -> Just (Atom.linkHref l)
            _ -> Nothing
    getRSSFeedAlternateLink rf = Nothing -- RSS channel link not easily accessible
