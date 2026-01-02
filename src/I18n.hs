{-# LANGUAGE OverloadedStrings #-}

module I18n where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Format (TimeLocale(..), defaultTimeLocale)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Text.Toml as Toml
import Data.Maybe (fromMaybe)

-- Configuration Types
data FeedType = Blog | YouTube | Flickr
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
  , configTimezone :: Text
  } deriving (Show)

-- App Data Types
data AppItem = AppItem
  { itemTitle :: Text
  , itemLink :: Text
  , itemDate :: Maybe UTCTime
  , itemDesc :: Maybe Text
  , itemThumbnail :: Maybe Text
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
    let timezone = fromMaybe "Europe/Helsinki" $ HM.lookup "timezone" toml >>= extractText
    
    feeds <- case HM.lookup "feeds" toml of
        Just (Toml.VTArray nodes) -> do
             configs <- mapM (parseFeedConfig . Toml.VTable) (V.toList nodes)
             return configs
        _ -> Right []

    return $ Config title feeds locale timezone
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
            "flickr" -> Right Flickr
            _ -> Left $ "Unknown feed type: " <> typeStr
            
        title <- case lookupKey "title" of
            Just (Toml.VString t) -> Right t
            _ -> Left "Missing or invalid feed title"
            
        url <- case lookupKey "url" of
            Just (Toml.VString t) -> Right t
            _ -> Left "Missing or invalid feed url"
            
        return $ FeedConfig ft title url
-- Supported locales
data Locale = En | Fi
  deriving (Show, Eq, Read)

-- Message keys
data Messages = Messages
  { msgLatestVideos :: Text
  , msgLatestPosts :: Text
  , msgGeneratedOn :: Text
  , msgPoweredBy :: Text
  , msgCookieConsentTitle :: Text
  , msgCookieConsentText :: Text
  , msgCookieConsentButton :: Text
  , msgCookieRejectButton :: Text
  , msgRevokeConsentTitle :: Text
  }

-- Translations
messagesEn :: Messages
messagesEn = Messages
  { msgLatestVideos = "Latest Videos"
  , msgLatestPosts = "Latest Posts"
  , msgGeneratedOn = "Generated on"
  , msgPoweredBy = "Powered by Haskell Planet Generator"
  , msgCookieConsentTitle = "Cookie Consent"
  , msgCookieConsentText = "This site uses external images that may track you. Do you agree to load them?"
  , msgCookieConsentButton = "I Agree"
  , msgCookieRejectButton = "Reject"
  , msgRevokeConsentTitle = "Revoke consent"
  }

messagesFi :: Messages
messagesFi = Messages
  { msgLatestVideos = "Uusimmat videot"
  , msgLatestPosts = "Uusimmat kirjoitukset"
  , msgGeneratedOn = "Koottu"
  , msgPoweredBy = "Suomen Palikkayhteisö ry:n RSS-kooste"
  , msgCookieConsentTitle = "Yksityisyys"
  , msgCookieConsentText = "Tämä sivu käyttää ulkoisia kuvia, jotka voivat seurata sinua. Sallitko niiden lataamisen?"
  , msgCookieConsentButton = "Salli"
  , msgCookieRejectButton = "Estä"
  , msgRevokeConsentTitle = "Muuta asetuksia"
  }

getMessages :: Locale -> Messages
getMessages En = messagesEn
getMessages Fi = messagesFi

getTimeLocale :: Locale -> TimeLocale
getTimeLocale En = defaultTimeLocale
getTimeLocale Fi = fiTimeLocale

defaultLocale :: Locale
defaultLocale = Fi

parseLocale :: Text -> Locale
parseLocale "en" = En
parseLocale "fi" = Fi
parseLocale _    = defaultLocale

fiTimeLocale :: TimeLocale
fiTimeLocale = defaultTimeLocale
    { wDays = 
        [ ("sunnuntai", "su")
        , ("maanantai", "ma")
        , ("tiistai", "ti")
        , ("keskiviikko", "ke")
        , ("torstai", "to")
        , ("perjantai", "pe")
        , ("lauantai", "la")
        ]
    , months = 
        [ ("tammikuu", "tammi")
        , ("helmikuu", "helmi")
        , ("maaliskuu", "maalis")
        , ("huhtikuu", "huhti")
        , ("toukokuu", "touko")
        , ("kesäkuu", "kesä")
        , ("heinäkuu", "heinä")
        , ("elokuu", "elo")
        , ("syyskuu", "syys")
        , ("lokakuu", "loka")
        , ("marraskuu", "marras")
        , ("joulukuu", "joulu")
        ]
    , amPm = ("ap.", "ip.")
    , dateTimeFmt = "%d.%m.%Y %H.%M.%S"
    , dateFmt = "%d.%m.%Y"
    , timeFmt = "%H.%M.%S"
    , time12Fmt = "%H.%M.%S"
    }
