{-# LANGUAGE OverloadedStrings #-}

module HtmlGen where

import Control.Monad (forM_)
import Data.Function (on)
import Data.List (groupBy)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, formatTime, TimeLocale)
import Data.Time.LocalTime (utcToZonedTime, TimeZone)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Data.ByteString.Lazy as LBS

import I18n
import Config
import Styles
import Scripts
import HtmlSanitizer

-- HTML Generation Components
renderHead :: Config -> H.Html
renderHead config = H.head $ do
    H.meta H.! A.charset "UTF-8"
    H.meta H.! A.name "viewport" H.! A.content "width=device-width, initial-scale=1.0"
    H.title (H.toHtml $ configTitle config)
    H.style $ H.toHtml css

renderCookieConsent :: Messages -> H.Html
renderCookieConsent msgs = H.div H.! A.id "cookie-consent" H.! A.class_ "cookie-consent hidden" $ do
    H.div H.! A.class_ "consent-content" $ do
        H.p $ H.toHtml (msgCookieConsentText msgs)
        -- Swapped Buttons: Consent first (primary), Reject second (secondary)
        H.button H.! A.id "consent-btn" $ H.toHtml (msgCookieConsentButton msgs)
        H.button H.! A.id "reject-btn" $ H.toHtml (msgCookieRejectButton msgs)

renderRevokeButton :: Messages -> H.Html
renderRevokeButton msgs = H.button H.! A.id "revoke-btn" H.! A.class_ "revoke-btn hidden" H.! A.title (H.toValue $ msgRevokeConsentTitle msgs) $ do
    "⚙️"

renderTimelineNav :: TimeLocale -> UTCTime -> TimeZone -> [(Text, Text, [AppItem])] -> H.Html
renderTimelineNav locale now localTZ groups = H.nav H.! A.class_ "timeline" $ do
    H.div H.! A.class_ "timeline-header" $ H.toHtml (formatTime locale "%Y" (utcToZonedTime localTZ now))
    H.ul $ forM_ groups $ \(monthLabel, monthId, _) -> do
        H.li $ H.a H.! A.href (H.toValue $ "#" <> monthId) $ H.toHtml monthLabel

renderIntro :: Config -> Messages -> TimeLocale -> UTCTime -> TimeZone -> H.Html
renderIntro config msgs locale now localTZ = H.div H.! A.class_ "intro" $ do
    H.h1 (H.toHtml $ configTitle config)
    H.p $ do
        H.toHtml (msgGeneratedOn msgs)
        " "
        H.toHtml (formatTime locale "%Y-%m-%d %H:%M:%S" (utcToZonedTime localTZ now))

renderMonthSection :: TimeLocale -> (Text, Text, [AppItem]) -> H.Html
renderMonthSection locale (monthLabel, monthId, groupItems) = H.div H.! A.id (H.toValue monthId) H.! A.class_ "month-section" $ do
    H.h2 H.! A.class_ "month-title" $ H.toHtml monthLabel
    H.div H.! A.class_ "grid" $ do
        mapM_ (renderCard locale) groupItems

renderFooter :: Messages -> H.Html
renderFooter msgs = H.footer $ do
    H.p (H.toHtml $ msgPoweredBy msgs)

renderScript :: H.Html
renderScript = H.script $ H.preEscapedToHtml js

-- HTML Generation
generateHtml :: Config -> Messages -> [AppItem] -> UTCTime -> TimeZone -> LBS.ByteString
generateHtml config msgs items now localTZ = renderHtml $ H.docTypeHtml $ do
    renderHead config
    H.body $ do
        renderCookieConsent msgs
        renderRevokeButton msgs
        H.div H.! A.class_ "layout" $ do
            renderTimelineNav locale now localTZ groups
            H.main H.! A.class_ "main-content" $ do
                renderIntro config msgs locale now localTZ
                forM_ groups (renderMonthSection locale)
        renderFooter msgs
        renderScript
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

renderCard :: TimeLocale -> AppItem -> H.Html
renderCard locale item = H.div H.! A.class_ "card" $ do
    case itemThumbnail item of
        Just url -> H.div H.! A.class_ "card-image" $ do
            H.img H.! A.class_ "lazy-consent" 
                  H.! H.dataAttribute "src" (H.toValue url) 
                  H.! A.alt (H.toValue $ itemTitle item)
        Nothing -> return ()
    H.div H.! A.class_ "card-content" $ do
        H.span H.! A.class_ "source" $ H.toHtml (itemSourceTitle item)
        H.h3 $ H.a H.! A.href (H.textValue $ itemLink item) H.! A.target "_blank" $ H.toHtml (itemTitle item)
        case itemDesc item of
             Just d -> H.div H.! A.class_ "description" $ H.preEscapedToHtml (cleanAndTruncate 256 d)
             Nothing -> return ()
        H.div H.! A.class_ "card-meta" $ do
            case itemDate item of
                Just d -> H.div H.! A.class_ "date" $ H.toHtml (formatTime locale "%Y-%m-%d" d)
                Nothing -> return ()
            H.div H.! A.class_ "type-icon" $ case itemType item of
                YouTube -> "🎥"
                Blog -> "📝"
                Flickr -> "📷"