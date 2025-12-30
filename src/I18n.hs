{-# LANGUAGE OverloadedStrings #-}

module I18n where

import Data.Text (Text)
import qualified Data.Text as T

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
  }

messagesFi :: Messages
messagesFi = Messages
  { msgLatestVideos = "Uusimmat videot"
  , msgLatestPosts = "Uusimmat kirjoitukset"
  , msgGeneratedOn = "Luotu"
  , msgPoweredBy = "Voimanlähteenä Haskell Planet Generator"
  , msgCookieConsentTitle = "Evästeasetukset"
  , msgCookieConsentText = "Tämä sivu käyttää ulkoisia kuvia, jotka voivat seurata sinua. Sallitko niiden lataamisen?"
  , msgCookieConsentButton = "Hyväksyn"
  }

getMessages :: Locale -> Messages
getMessages En = messagesEn
getMessages Fi = messagesFi

defaultLocale :: Locale
defaultLocale = Fi

parseLocale :: Text -> Locale
parseLocale "en" = En
parseLocale "fi" = Fi
parseLocale _    = defaultLocale
