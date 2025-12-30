{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (try, SomeException)
import Control.Monad (forM, when)
import qualified Data.ByteString.Lazy as LBS
import Data.List (sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime, diffUTCTime)
import qualified Text.Toml as Toml
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Text.Feed.Import (parseFeedSource)
import Text.Feed.Query (getFeedItems, getItemTitle, getItemLink, getItemPublishDate, getItemDescription)
import Text.Feed.Types (Feed, Item(..))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Network.HTTP.Simple (httpLBS, getResponseBody, parseRequest, Response)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Text.HTML.TagSoup (parseTags, renderTags, Tag(..))

import I18n

-- Configuration Types
data FeedType = Blog | YouTube
  deriving (Show, Eq)

data FeedConfig = FeedConfig
  { feedType :: FeedType
  , feedTitle :: Text
  , feedUrl :: Text
  } deriving (Show)

data Config = Config
  { configTitle :: Text
  , configFeeds :: [FeedConfig]
  , configLocale :: Locale
  } deriving (Show)

-- App Data Types
data AppItem = AppItem
  { itemTitle :: Text
  , itemLink :: Text
  , itemDate :: Maybe UTCTime
  , itemDesc :: Maybe Text
  , itemSourceTitle :: Text
  , itemType :: FeedType
  } deriving (Show)

-- TOML Parsing helpers
parseConfig :: Text -> Either Text Config
parseConfig content = do
    toml <- case Toml.parseTomlDoc "config.toml" content of
        Left err -> Left $ T.pack $ show err
        Right t -> Right t
    
    let title = fromMaybe "Planet" $ HM.lookup "title" toml >>= extractText
    let localeStr = fromMaybe "fi" $ HM.lookup "locale" toml >>= extractText
    let locale = parseLocale localeStr
    
    feeds <- case HM.lookup "feeds" toml of
        Just (Toml.VTArray nodes) -> do
             configs <- mapM (parseFeedConfig . Toml.VTable) (V.toList nodes)
             return configs
        _ -> Right []

    return $ Config title feeds locale
  where
    extractText (Toml.VString t) = Just t
    extractText _ = Nothing

    parseFeedConfig node = do
        let lookupKey k = case node of
                Toml.VTable t -> HM.lookup k t
                _ -> Nothing
        
        typeStr <- case lookupKey "type" of
            Just (Toml.VString t) -> Right t
            _ -> Left "Missing or invalid feed type"
            
        ft <- case typeStr of
            "blog" -> Right Blog
            "youtube" -> Right YouTube
            _ -> Left $ "Unknown feed type: " <> typeStr
            
        title <- case lookupKey "title" of
            Just (Toml.VString t) -> Right t
            _ -> Left "Missing or invalid feed title"
            
        url <- case lookupKey "url" of
            Just (Toml.VString t) -> Right t
            _ -> Left "Missing or invalid feed url"
            
        return $ FeedConfig ft title url

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

parseItem :: FeedConfig -> Item -> Maybe AppItem
parseItem fc item = do
    title <- getItemTitle item
    link <- getItemLink item
    let date = join $ getItemPublishDate item
    let desc = getItemDescription item
    return $ AppItem title link date desc (feedTitle fc) (feedType fc)

-- Helper for date join
join :: Maybe (Maybe a) -> Maybe a
join (Just (Just x)) = Just x
join _ = Nothing

-- HTML Processing for Consent
processHtml :: Text -> Text
processHtml rawHtml = renderTags $ map processTag (parseTags rawHtml)
  where
    processTag (TagOpen "img" attrs) = TagOpen "img" (modifyImgAttrs attrs)
    processTag other = other

    modifyImgAttrs :: [(Text, Text)] -> [(Text, Text)]
    modifyImgAttrs attrs =
        let src = fromMaybe "" $ lookup "src" attrs
            otherAttrs = filter (\(k, _) -> k /= "src" && k /= "class") attrs
            classes = fromMaybe "" $ lookup "class" attrs
        in ("data-src", src) : ("class", "lazy-consent " <> classes) : otherAttrs

-- HTML Generation
generateHtml :: Config -> Messages -> [AppItem] -> UTCTime -> LBS.ByteString
generateHtml config msgs items now = renderHtml $ H.docTypeHtml $ do
    H.head $ do
        H.meta H.! A.charset "UTF-8"
        H.meta H.! A.name "viewport" H.! A.content "width=device-width, initial-scale=1.0"
        H.title (H.toHtml $ configTitle config)
        H.style $ H.toHtml css
    H.body $ do
        -- Cookie Consent Banner
        H.div H.! A.id "cookie-consent" H.! A.class_ "cookie-consent hidden" $ do
            H.div H.! A.class_ "consent-content" $ do
                H.h3 $ H.toHtml (msgCookieConsentTitle msgs)
                H.p $ H.toHtml (msgCookieConsentText msgs)
                H.button H.! A.id "consent-btn" $ H.toHtml (msgCookieConsentButton msgs)

        H.header $ do
            H.h1 (H.toHtml $ configTitle config)
            H.p $ do
                H.toHtml (msgGeneratedOn msgs)
                " "
                H.toHtml (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC" now)
        
        H.main $ do
            H.section H.! A.class_ "feed-section" $ do
                H.h2 (H.toHtml $ msgLatestVideos msgs)
                H.div H.! A.class_ "grid" $ do
                    mapM_ renderCard (take 12 $ filter (\i -> itemType i == YouTube) items)

            H.section H.! A.class_ "feed-section" $ do
                H.h2 (H.toHtml $ msgLatestPosts msgs)
                H.div H.! A.class_ "list" $ do
                    mapM_ renderListItem (take 20 $ filter (\i -> itemType i == Blog) items)
        
        H.footer $ do
            H.p (H.toHtml $ msgPoweredBy msgs)

        -- Script for Cookie Consent & Lazy Loading
        H.script $ H.preEscapedToHtml js

renderCard :: AppItem -> H.Html
renderCard item = H.div H.! A.class_ "card" $ do
    H.div H.! A.class_ "card-content" $ do
        H.span H.! A.class_ "source" $ H.toHtml (itemSourceTitle item)
        H.h3 $ H.a H.! A.href (H.textValue $ itemLink item) H.! A.target "_blank" $ H.toHtml (itemTitle item)
        case itemDate item of
            Just d -> H.div H.! A.class_ "date" $ H.toHtml (formatTime defaultTimeLocale "%Y-%m-%d" d)
            Nothing -> return ()

renderListItem :: AppItem -> H.Html
renderListItem item = H.div H.! A.class_ "list-item" $ do
    H.div H.! A.class_ "list-item-header" $ do
        H.span H.! A.class_ "source" $ H.toHtml (itemSourceTitle item)
        case itemDate item of
            Just d -> H.span H.! A.class_ "date" $ H.toHtml (formatTime defaultTimeLocale "%Y-%m-%d" d)
            Nothing -> return ()
    H.h3 $ H.a H.! A.href (H.textValue $ itemLink item) H.! A.target "_blank" $ H.toHtml (itemTitle item)
    case itemDesc item of
        Just d -> H.div H.! A.class_ "description" $ H.preEscapedToHtml (processHtml d)
        Nothing -> return ()

-- Embedded CSS (Elegant & Minimal)
css :: Text
css = T.unlines
    [ "body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; line-height: 1.6; color: #333; max-width: 1200px; margin: 0 auto; padding: 20px; background-color: #f9f9f9; }"
    , "header { text-align: center; margin-bottom: 40px; padding: 20px 0; border-bottom: 1px solid #eaeaea; }"
    , "h1 { font-weight: 700; color: #111; margin: 0; }"
    , "h2 { font-weight: 600; color: #444; border-bottom: 2px solid #333; padding-bottom: 10px; margin-top: 40px; }"
    , ".grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(300px, 1fr)); gap: 20px; }"
    , ".card { background: white; border: 1px solid #eaeaea; border-radius: 8px; padding: 20px; transition: box-shadow 0.2s; display: flex; flex-direction: column; }"
    , ".card:hover { box-shadow: 0 4px 12px rgba(0,0,0,0.05); }"
    , ".card-content { display: flex; flex-direction: column; height: 100%; }"
    , ".source { font-size: 0.85rem; color: #666; text-transform: uppercase; letter-spacing: 0.5px; font-weight: 600; margin-bottom: 8px; display: block; }"
    , ".card h3 { margin: 0 0 10px 0; font-size: 1.1rem; flex-grow: 1; }"
    , ".card a { color: #111; text-decoration: none; }"
    , ".card a:hover { color: #0070f3; }"
    , ".date { font-size: 0.85rem; color: #888; margin-top: auto; }"
    , ".list { display: flex; flex-direction: column; gap: 15px; }"
    , ".list-item { background: white; border: 1px solid #eaeaea; border-radius: 8px; padding: 20px; }"
    , ".list-item-header { display: flex; justify-content: space-between; align-items: baseline; margin-bottom: 8px; }"
    , ".list-item h3 { margin: 0 0 10px 0; font-size: 1.2rem; }"
    , ".list-item a { color: #111; text-decoration: none; }"
    , ".list-item a:hover { color: #0070f3; }"
    , ".description { font-size: 0.95rem; color: #555; margin: 0; overflow: hidden; }"
    , ".description img { max-width: 100%; height: auto; display: none; }" -- Hide images by default via CSS too
    , ".description img.loaded { display: block; margin: 10px 0; }"
    , "footer { margin-top: 60px; text-align: center; color: #888; font-size: 0.9rem; border-top: 1px solid #eaeaea; padding-top: 20px; }"
    , "@media (max-width: 600px) { .grid { grid-template-columns: 1fr; } }"
    -- Cookie Consent CSS
    , ".cookie-consent { position: fixed; bottom: 0; left: 0; right: 0; background: #222; color: white; padding: 20px; display: flex; justify-content: center; align-items: center; z-index: 1000; }"
    , ".cookie-consent.hidden { display: none; }"
    , ".consent-content { display: flex; align-items: center; gap: 20px; max-width: 1200px; }"
    , ".consent-content h3 { margin: 0; color: white; border: none; padding: 0; font-size: 1.1rem; }"
    , ".consent-content p { margin: 0; font-size: 0.9rem; }"
    , "#consent-btn { background: #0070f3; color: white; border: none; padding: 10px 20px; border-radius: 5px; cursor: pointer; font-weight: 600; }"
    , "#consent-btn:hover { background: #0051a2; }"
    ]

js :: Text
js = T.unlines
    [ "document.addEventListener('DOMContentLoaded', function() {"
    , "  var consentBanner = document.getElementById('cookie-consent');"
    , "  var consentBtn = document.getElementById('consent-btn');"
    , "  var images = document.querySelectorAll('img.lazy-consent');"
    , ""
    , "  function loadImages() {"
    , "    images.forEach(function(img) {"
    , "      if (img.dataset.src) {"
    , "        img.src = img.dataset.src;"
    , "        img.classList.add('loaded');"
    , "        img.classList.remove('lazy-consent');"
    , "      }"
    , "    });"
    , "  }"
    , ""
    , "  if (localStorage.getItem('image-consent') === 'true') {"
    , "    loadImages();"
    , "  } else {"
    , "    consentBanner.classList.remove('hidden');"
    , "  }"
    , ""
    , "  consentBtn.addEventListener('click', function() {"
    , "    localStorage.setItem('image-consent', 'true');"
    , "    consentBanner.classList.add('hidden');"
    , "    loadImages();"
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
            let html = generateHtml config messages sortedItems now
            
            createDirectoryIfMissing True "public"
            LBS.writeFile "public/index.html" html
            putStrLn "Site generated in public/index.html"