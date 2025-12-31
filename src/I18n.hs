{-# LANGUAGE OverloadedStrings #-}

module I18n where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Format (TimeLocale(..), defaultTimeLocale)

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
