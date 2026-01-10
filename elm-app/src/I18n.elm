module I18n exposing (MsgKey(..), translate)

{-| Internationalization module

@docs MsgKey, translate

-}

import Types exposing (Lang)


{-| Translation keys
-}
type MsgKey
    = SkipToContent
    | Close
    | Open
    | CloseMenu
    | OpenMenu
    | Timeline
    | Search
    | SearchPlaceholder
    | FeedFilters
    | Filters
    | View
    | Descriptions
    | Title
    | Compiled
    | Description
    | DownloadOpml
    | ScrollToTop


{-| Translate a key to the given language
-}
translate : Types.Lang -> MsgKey -> String
translate lang key =
    case lang of
        Types.Fi ->
            case key of
                SkipToContent ->
                    "Siirry pääsisältöön"

                Close ->
                    "✕"

                Open ->
                    "≡"

                CloseMenu ->
                    "Sulje valikko"

                OpenMenu ->
                    "Avaa valikko"

                Timeline ->
                    "Aikajana"

                Search ->
                    "Haku"

                SearchPlaceholder ->
                    "Hae..."

                FeedFilters ->
                    "Feed filters"

                Filters ->
                    "Suodattimet"

                View ->
                    "Näkymä"

                Descriptions ->
                    "👁️ Kuvaukset"

                Title ->
                    "Palikkalinkit"

                Compiled ->
                    "Koottu "

                Description ->
                    "Suomen Palikkayhteisö ry:n tuottama syötekooste"

                DownloadOpml ->
                    "Lataa OPML"

                ScrollToTop ->
                    "Siirry ylös"

        Types.En ->
            case key of
                SkipToContent ->
                    "Skip to main content"

                Close ->
                    "✕"

                Open ->
                    "≡"

                CloseMenu ->
                    "Close menu"

                OpenMenu ->
                    "Open menu"

                Timeline ->
                    "Timeline"

                Search ->
                    "Search"

                SearchPlaceholder ->
                    "Search..."

                FeedFilters ->
                    "Feed filters"

                Filters ->
                    "Filters"

                View ->
                    "View"

                Descriptions ->
                    "👁️ Descriptions"

                Title ->
                    "Palikkalinkit"

                Compiled ->
                    "Compiled "

                Description ->
                    "Feed aggregator produced by Suomen Palikkayhteisö ry"

                DownloadOpml ->
                    "Download OPML"

                ScrollToTop ->
                    "Scroll to top"