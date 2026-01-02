{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Planet where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (try, SomeException)
import Control.Monad (forM_, mplus)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isSpace)
import Data.List (sortOn, groupBy)
import Data.Function (on)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, formatTime, getCurrentTime, TimeLocale)
import Data.Time.LocalTime (utcToZonedTime, TimeZone)
import Data.Time.Zones (timeZoneForUTCTime)
import Data.Time.Zones.All (tzByLabel, fromTZName)
import qualified Text.Toml as Toml
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Text.Feed.Import (parseFeedSource)
import Text.Feed.Query (getFeedItems, getItemTitle, getItemLink, getItemPublishDate, getItemDescription)
import Text.Feed.Types (Item(..))
import qualified Text.Atom.Feed as Atom
import qualified Text.RSS.Syntax as RSS
import Data.XML.Types (Element(..), Name(..), Node(..), Content(..))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Network.HTTP.Simple (httpLBS, getResponseBody, parseRequest, Response)
import System.Directory (createDirectoryIfMissing)
import Text.HTML.TagSoup (parseTags, renderTags, Tag(..))
import Text.HTML.TagSoup.Tree (TagTree(..), tagTree, flattenTree)
import Debug.Trace (traceShow)

import I18n

-- Configuration Types
-- Types moved to I18n

-- App Data Types
-- Types moved to I18n

-- TOML Parsing helpers
-- Moved to I18n

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
                Just feed -> return $ mapMaybe (parseItem fc) (getFeedItems feed)

-- Media Description Extraction
getMediaDescription :: [Element] -> Maybe Text
getMediaDescription elements =
    -- First check for descriptions directly in the elements
    case filter isMediaDescription elements of
        (d:_) -> elementText d
        [] -> 
            -- Then check inside media groups
            let mediaGroups = filter isMediaGroup elements
            in case mediaGroups of
                (g:_) -> 
                    let descriptionElements = filter isMediaDescription (elementNodes g >>= nodeToElem)
                    in case descriptionElements of
                        (d:_) -> elementText d
                        _ -> Nothing
                _ -> Nothing
  where
    nodeToElem (NodeElement e) = [e]
    nodeToElem _ = []
    
    elementText e = 
        let texts = [t | NodeContent (ContentText t) <- elementNodes e]
        in if null texts then Nothing else Just (T.concat texts)

isMediaDescription :: Element -> Bool
isMediaDescription e = nameLocalName (elementName e) == "description" 
                    && nameNamespace (elementName e) == Just "http://search.yahoo.com/mrss/"

parseItem :: FeedConfig -> Item -> Maybe AppItem
parseItem fc item = do
    title <- getItemTitle item
    link <- getItemLink item
    let date = join $ getItemPublishDate item
    let defaultDesc = getItemDescription item
    let youtubeMediaDesc = case (item, feedType fc) of
                            (AtomItem entry, YouTube) -> getMediaDescription (Atom.entryOther entry)
                            (XMLItem element, YouTube) -> getMediaDescription (elementChildren element)
                            _ -> Nothing
    let desc = youtubeMediaDesc `mplus` defaultDesc
    let thumb = getItemThumbnail item `mplus` (desc >>= extractFirstImage)
    return $ AppItem title link date desc thumb (feedTitle fc) (feedType fc)

extractFirstImage :: Text -> Maybe Text
extractFirstImage html = 
    let tags = parseTags html
        imgTags = filter (\t -> case t of TagOpen "img" _ -> True; _ -> False) tags
    in case imgTags of
        (TagOpen "img" attrs : _) -> lookup "src" attrs
        _ -> Nothing

-- Helper for date join
join :: Maybe (Maybe a) -> Maybe a
join (Just (Just x)) = Just x
join _ = Nothing

-- Thumbnail extraction
getItemThumbnail :: Item -> Maybe Text
getItemThumbnail item = case item of
    AtomItem entry -> getAtomThumbnail entry
    RSSItem rssItem -> getRSSThumbnail rssItem
    XMLItem element -> findMediaThumbnail (elementChildren element) `mplus` findMediaGroupThumbnail (elementChildren element)
    _ -> Nothing

elementChildren :: Element -> [Element]
elementChildren e = [c | NodeElement c <- elementNodes e]

showItemType :: Item -> String
showItemType (AtomItem _) = "Atom"
showItemType (RSSItem _) = "RSS"
showItemType (RSS1Item _) = "RSS1"
showItemType (XMLItem _) = "XML"

getAtomThumbnail :: Atom.Entry -> Maybe Text
getAtomThumbnail entry = 
    let others = Atom.entryOther entry
    in traceShow ("Entry: " <> show (Atom.entryTitle entry)) $
       traceShow ("Elements: " <> show others) $
       case findMediaThumbnail others of
        Just url -> Just url
        Nothing -> findMediaGroupThumbnail others

getRSSThumbnail :: RSS.RSSItem -> Maybe Text
getRSSThumbnail item = 
    case findMediaThumbnail (RSS.rssItemOther item) of
        Just url -> Just url
        Nothing -> findMediaGroupThumbnail (RSS.rssItemOther item)

findMediaThumbnail :: [Element] -> Maybe Text
findMediaThumbnail elems = 
    case filter isMediaThumbnail elems of
        (e:_) -> getUrlAttr e
        [] -> Nothing

findMediaGroupThumbnail :: [Element] -> Maybe Text
findMediaGroupThumbnail elems = 
    case filter isMediaGroup elems of
        (g:_) -> findMediaThumbnail (elementNodes g >>= nodeToElem)
        [] -> Nothing
  where
    nodeToElem (NodeElement e) = [e]
    nodeToElem _ = []

isMediaThumbnail :: Element -> Bool
isMediaThumbnail e = nameLocalName (elementName e) == "thumbnail" 
                  && nameNamespace (elementName e) == Just "http://search.yahoo.com/mrss/"

isMediaGroup :: Element -> Bool
isMediaGroup e = nameLocalName (elementName e) == "group" 
              && nameNamespace (elementName e) == Just "http://search.yahoo.com/mrss/"

getUrlAttr :: Element -> Maybe Text
getUrlAttr e = 
    case filter (\(n, _) -> nameLocalName n == "url") (elementAttributes e) of
        ((_, [ContentText t]):_) -> Just t
        _ -> Nothing

-- HTML Generation
generateHtml :: Config -> Messages -> [AppItem] -> UTCTime -> TimeZone -> LBS.ByteString
generateHtml config msgs items now localTZ = renderHtml $ H.docTypeHtml $ do
    H.head $ do
        H.meta H.! A.charset "UTF-8"
        H.meta H.! A.name "viewport" H.! A.content "width=device-width, initial-scale=1.0"
        H.title (H.toHtml $ configTitle config)
        H.style $ H.toHtml css
    H.body $ do
        -- Cookie Consent Banner
        H.div H.! A.id "cookie-consent" H.! A.class_ "cookie-consent hidden" $ do
            H.div H.! A.class_ "consent-content" $ do
                H.p $ H.toHtml (msgCookieConsentText msgs)
                -- Swapped Buttons: Consent first (primary), Reject second (secondary)
                H.button H.! A.id "consent-btn" $ H.toHtml (msgCookieConsentButton msgs)
                H.button H.! A.id "reject-btn" $ H.toHtml (msgCookieRejectButton msgs)

        -- Revoke Consent Button
        H.button H.! A.id "revoke-btn" H.! A.class_ "revoke-btn hidden" H.! A.title (H.toValue $ msgRevokeConsentTitle msgs) $ do
            "⚙️"

        H.div H.! A.class_ "layout" $ do
            -- Timeline Navigation
            H.nav H.! A.class_ "timeline" $ do
                H.div H.! A.class_ "timeline-header" $ H.toHtml (formatTime locale "%Y" (utcToZonedTime localTZ now))
                H.ul $ forM_ groups $ \(monthLabel, monthId, _) -> do
                    H.li $ H.a H.! A.href (H.toValue $ "#" <> monthId) $ H.toHtml monthLabel

            H.main H.! A.class_ "main-content" $ do
                -- Intro / Title area within main since header is gone
                H.div H.! A.class_ "intro" $ do
                    H.h1 (H.toHtml $ configTitle config)
                    H.p $ do
                        H.toHtml (msgGeneratedOn msgs)
                        " "
                        H.toHtml (formatTime locale "%Y-%m-%d %H:%M:%S" (utcToZonedTime localTZ now))

                forM_ groups $ \(monthLabel, monthId, groupItems) -> do
                    H.div H.! A.id (H.toValue monthId) H.! A.class_ "month-section" $ do
                        H.h2 H.! A.class_ "month-title" $ H.toHtml monthLabel
                        H.div H.! A.class_ "grid" $ do
                            mapM_ (renderCard locale) groupItems
        
        H.footer $ do
            H.p (H.toHtml $ msgPoweredBy msgs)

        -- Script for Cookie Consent & Lazy Loading
        H.script $ H.preEscapedToHtml js
  where
    locale = getTimeLocale (configLocale config)

    -- Group items by Year-Month
    groups = map mkGroup $ groupBy ((==) `on` itemMonth) items
    
    itemMonth item = case itemDate item of
        Just d -> formatTime locale "%Y-%m" d
        Nothing -> "Unknown"

    mkGroup groupItems = 
        let first = head groupItems
            monthLabel = case itemDate first of
                Just d -> T.pack $ formatTime locale "%B %Y" d
                Nothing -> "Older / Undated"
            monthId = case itemDate first of
                Just d -> T.pack $ formatTime locale "m-%Y-%m" d
                Nothing -> "m-unknown"
        in (monthLabel, monthId, groupItems)

cleanAndTruncate :: Int -> Text -> Text
cleanAndTruncate maxLength html =
    let tags = parseTags html
        normalized = normalizeVoids tags
        tree = tagTree normalized
        pruned = pruneTree tree
        flat = flattenTree pruned
    in renderTags $ takeWithLimit maxLength [] flat

-- Helper to ensure void tags are properly closed for tree construction
normalizeVoids :: [Tag Text] -> [Tag Text]
normalizeVoids [] = []
normalizeVoids (TagOpen name attrs : rest)
    | name `elem` voidTags =
        case rest of
            (TagClose name2 : rest2) | name == name2 ->
                TagOpen name attrs : TagClose name : normalizeVoids rest2
            _ ->
                TagOpen name attrs : TagClose name : normalizeVoids rest
normalizeVoids (x:xs) = x : normalizeVoids xs

pruneTree :: [TagTree Text] -> [TagTree Text]
pruneTree = filter (not . isEmptyTree) . filter (not . isSeparator) . filter (not . isImageTree) . map pruneBranch
  where
    pruneBranch (TagBranch name attrs children) = TagBranch name attrs (pruneTree children)
    pruneBranch leaf = leaf

    isSeparator (TagBranch _ attrs _) = 
        case lookup "class" attrs of
            Just cls -> "separator" `elem` T.words cls
            Nothing -> False
    isSeparator _ = False

    isImageTree (TagBranch name _ _) = name == "img"
    isImageTree _ = False

    isEmptyTree (TagBranch name _ []) = not (name `elem` voidTags)
    isEmptyTree (TagBranch name _ _) | name `elem` ["script", "style"] = True
    isEmptyTree (TagBranch _ _ children) = all isEmptyTree children
    isEmptyTree (TagLeaf tag) = not (isVisibleTag tag)

    isVisibleTag (TagText t) = not (T.all isSpace t)
    isVisibleTag (TagOpen name _) = name `elem` voidTags
    isVisibleTag _ = False

voidTags :: [Text]
voidTags = ["area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param", "source", "track", "wbr", "video", "audio", "iframe", "object", "svg"]

takeWithLimit :: Int -> [Text] -> [Tag Text] -> [Tag Text]
takeWithLimit _ stack [] = map TagClose stack
takeWithLimit remainingLen stack (t:ts)
    | remainingLen <= 0 = map TagClose stack
    | otherwise = case t of
        TagText text ->
            let len = T.length text
            in if len <= remainingLen
               then t : takeWithLimit (remainingLen - len) stack ts
               else [TagText (T.take remainingLen text <> "...")] ++ map TagClose stack
        TagOpen name _ ->
            if name `elem` voidTags
            then t : takeWithLimit remainingLen stack ts
            else t : takeWithLimit remainingLen (name : stack) ts
        TagClose name ->
            if not (null stack) && head stack == name
            then t : takeWithLimit remainingLen (tail stack) ts
            else t : takeWithLimit remainingLen stack ts
        _ -> t : takeWithLimit remainingLen stack ts

renderCard :: TimeLocale -> AppItem -> H.Html
renderCard locale item = H.div H.! A.class_ "card" $ do
    case itemThumbnail item of
        Just url -> H.div H.! A.class_ "card-image" $ do
            H.img H.! A.class_ "lazy-consent" 
                  H.! H.dataAttribute "src" (H.toValue url) 
                  H.! A.alt (H.toValue $ itemTitle item)
            H.div H.! A.class_ "type-icon" $ case itemType item of
                YouTube -> "🎥"
                Blog -> "📝"
        Nothing -> return ()
    H.div H.! A.class_ "card-content" $ do
        H.span H.! A.class_ "source" $ H.toHtml (itemSourceTitle item)
        H.h3 $ H.a H.! A.href (H.textValue $ itemLink item) H.! A.target "_blank" $ H.toHtml (itemTitle item)
        case itemDesc item of
             Just d -> H.div H.! A.class_ "description" $ H.preEscapedToHtml (cleanAndTruncate 256 d)
             Nothing -> return ()
        case itemDate item of
            Just d -> H.div H.! A.class_ "date" $ H.toHtml (formatTime locale "%Y-%m-%d" d)
            Nothing -> return ()



-- Embedded CSS (Elegant & Minimal)
css :: Text
css = T.unlines
    [ "body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; line-height: 1.6; color: #333; margin: 0; padding: 0; background-color: #f9f9f9; }"
    , ".layout { display: flex; max-width: 1400px; margin: 0 auto; padding: 20px; gap: 40px; }"
    
    -- Timeline Navigation
    , ".timeline { width: 180px; flex-shrink: 0; position: sticky; top: 20px; align-self: start; height: calc(100vh - 40px); overflow-y: auto; padding-right: 10px; }"
    , ".timeline-header { font-weight: 900; font-size: 2rem; color: #eaeaea; margin-bottom: 20px; text-align: right; line-height: 1; }"
    , ".timeline ul { list-style: none; padding: 0; margin: 0; text-align: right; }"
    , ".timeline li { margin-bottom: 12px; }"
    , ".timeline a { text-decoration: none; color: #888; font-weight: 500; font-size: 0.95rem; transition: color 0.2s; display: block; padding: 4px 10px 4px 0; border-right: 2px solid transparent; }"
    , ".timeline a:hover { color: #111; border-right-color: #ddd; }"
    
    -- Main Content
    , ".main-content { flex-grow: 1; min-width: 0; }" -- min-width 0 prevents flex overflow
    , ".intro { }"
    , "h1 { font-weight: 700; color: #111; margin: 0 0 10px 0; font-size: 2.5rem; }"
    , "h2.month-title { font-weight: 600; color: #444; border-bottom: 2px solid #333; padding-bottom: 10px; margin: 40px 0 20px 0; font-size: 1.5rem; }"
    
    -- Grid & Cards
    , ".grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(300px, 1fr)); gap: 20px; }"
    , ".card { background: white; border: 1px solid #eaeaea; border-radius: 8px; padding: 20px; transition: box-shadow 0.2s; display: flex; flex-direction: column; }"
    , ".card:hover { box-shadow: 0 4px 12px rgba(0,0,0,0.05); }"
    , ".card-content { display: flex; flex-direction: column; height: 100%; }"
    , ".source { font-size: 0.85rem; color: #666; text-transform: uppercase; letter-spacing: 0.5px; font-weight: 600; margin-bottom: 8px; display: block; }"
    , ".card-image { width: 100%; height: 180px; overflow: hidden; background: #eee; margin-bottom: 15px; border-radius: 4px; position: relative; }"
    , ".card-image img { width: 100%; height: 100%; object-fit: cover; display: none; }"
    , ".card-image img.loaded { display: block; }"
    , ".type-icon { position: absolute; bottom: 8px; right: 8px; background: rgba(255,255,255,0.6); color: white; padding: 4px 6px; border-radius: 4px; font-size: 0.8rem; line-height: 1; pointer-events: none; }"
    , ".card h3 { margin: 0 0 10px 0; font-size: 1.1rem; line-height: 1.4; }"
    , ".card a { color: #111; text-decoration: none; }"
    , ".card a:hover { color: #0070f3; }"
    , ".date { font-size: 0.85rem; color: #888; margin-top: auto; padding-top: 15px; }"
    , ".description { font-size: 0.95rem; color: #555; margin: 0; overflow: hidden; }"
    , ".description img { max-width: 100%; height: auto; display: none; }"
    , ".description img.loaded { display: block; margin: 10px 0; }"
    
    , "footer { margin-top: 80px; text-align: center; color: #888; font-size: 0.9rem; border-top: 1px solid #eaeaea; padding-top: 20px; }"
    
    -- Responsive
    , "@media (max-width: 800px) {"
    , "  .layout { flex-direction: column; gap: 20px; }"
    , "  .timeline { width: 100%; height: auto; position: static; overflow-x: auto; padding-right: 0; border-bottom: 1px solid #eaeaea; padding-bottom: 10px; }"
    , "  .timeline-header { text-align: left; display: none; }"
    , "  .timeline ul { display: flex; text-align: left; gap: 15px; white-space: nowrap; }"
    , "  .timeline a { padding: 5px; border-right: none; border-bottom: 2px solid transparent; }"
    , "  .timeline a:hover { border-bottom-color: #333; }"
    , "}"
    , "@media (max-width: 600px) { .grid { grid-template-columns: 1fr; } }"

    -- Cookie Consent CSS
    , ".cookie-consent { position: fixed; bottom: 0; left: 0; right: 0; background: #222; color: white; padding: 20px; display: flex; justify-content: center; align-items: center; z-index: 1000; }"
    , ".cookie-consent.hidden { display: none; }"
    , ".consent-content { display: flex; align-items: center; gap: 20px; max-width: 1200px; flex-wrap: wrap; justify-content: center; }"
    , ".consent-content p { margin: 0; font-size: 0.9rem; }"
    
    -- Swapped Button Styles
    -- Consent: Primary (Blue)
    , "#consent-btn { background: #0070f3; color: white; border: none; padding: 10px 20px; border-radius: 5px; cursor: pointer; font-weight: 600; margin-left: 10px; }"
    , "#consent-btn:hover { background: #0051a2; }"
    -- Reject: Secondary (Transparent/White Border)
    , "#reject-btn { background: transparent; color: white; border: 1px solid white; padding: 10px 20px; border-radius: 5px; cursor: pointer; font-weight: 600; }"
    , "#reject-btn:hover { background: rgba(255,255,255,0.1); }"
    
    -- Revoke Button CSS
    , ".revoke-btn { position: fixed; bottom: 20px; right: 20px; background: rgba(255,255,255,0.8); border: 1px solid #ccc; border-radius: 50%; width: 40px; height: 40px; font-size: 20px; cursor: pointer; z-index: 999; display: flex; justify-content: center; align-items: center; box-shadow: 0 2px 5px rgba(0,0,0,0.1); }"
    , ".revoke-btn:hover { background: white; }"
    , ".revoke-btn.hidden { display: none; }"
    ]

js :: Text
js = T.unlines
    [ "document.addEventListener('DOMContentLoaded', function() {"
    , "  var consentBanner = document.getElementById('cookie-consent');"
    , "  var consentBtn = document.getElementById('consent-btn');"
    , "  var rejectBtn = document.getElementById('reject-btn');"
    , "  var revokeBtn = document.getElementById('revoke-btn');"
    , ""
    , "  function getAllImages() {"
    , "    return document.querySelectorAll('img.lazy-consent, img.loaded');"
    , "  }"
    , ""
    , "  function loadImages() {"
    , "    getAllImages().forEach(function(img) {"
    , "      if (img.dataset.src) {"
    , "        img.src = img.dataset.src;"
    , "        img.classList.add('loaded');"
    , "        img.classList.remove('lazy-consent');"
    , "      }"
    , "    });"
    , "  }"
    , ""
    , "  function hideImages() {"
    , "    getAllImages().forEach(function(img) {"
    , "      img.removeAttribute('src');"
    , "      img.classList.remove('loaded');"
    , "      img.classList.add('lazy-consent');"
    , "    });"
    , "  }"
    , ""
    , "  var consent = localStorage.getItem('image-consent');"
    , "  if (consent === 'true') {"
    , "    loadImages();"
    , "    revokeBtn.classList.remove('hidden');"
    , "  } else if (consent === 'false') {"
    , "    revokeBtn.classList.remove('hidden');"
    , "  } else {"
    , "    consentBanner.classList.remove('hidden');"
    , "  }"
    , ""
    , "  consentBtn.addEventListener('click', function() {"
    , "    localStorage.setItem('image-consent', 'true');"
    , "    consentBanner.classList.add('hidden');"
    , "    revokeBtn.classList.remove('hidden');"
    , "    loadImages();"
    , "  });"
    , ""
    , "  rejectBtn.addEventListener('click', function() {"
    , "    localStorage.setItem('image-consent', 'false');"
    , "    consentBanner.classList.add('hidden');"
    , "    revokeBtn.classList.remove('hidden');"
    , "    hideImages();"
    , "  });"
    , ""
    , "  revokeBtn.addEventListener('click', function() {"
    , "    localStorage.removeItem('image-consent');"
    , "    revokeBtn.classList.add('hidden');"
    , "    consentBanner.classList.remove('hidden');"
    , "    hideImages();"
    , "  });"
    , "});"
    ]

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
            let sortedItems = sortOn (\i -> Down (itemDate i)) allItems
            
            now <- getCurrentTime
            let messages = getMessages (configLocale config)
            
            -- Resolve TimeZone
            let tzName = TE.encodeUtf8 (configTimezone config)
            let localTZ = case fromTZName tzName of
                            Just label -> timeZoneForUTCTime (tzByLabel label) now
                            Nothing -> error $ "Unknown or invalid timezone: " ++ T.unpack (configTimezone config)
            
            let html = generateHtml config messages sortedItems now localTZ
            
            createDirectoryIfMissing True "public"
            LBS.writeFile "public/index.html" html
            putStrLn "Site generated in public/index.html"
