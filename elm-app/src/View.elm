module View exposing (view)

{-| View rendering for the application

@docs view

-}

import Data exposing (AppItem, FeedType(..))
import DateUtils exposing (formatDate)
import Html exposing (Html, a, button, div, footer, h1, h2, h3, img, input, label, li, main_, nav, p, span, text, ul)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Keyed
import Html.Lazy exposing (lazy2)
import Types exposing (Model, MonthGroup, Msg(..), ViewMode(..))


{-| Main view function
-}
view : Model -> Html Msg
view model =
    div [ Attr.class "min-h-screen bg-gray-50" ]
        [ -- Skip to content link for accessibility
          a
            [ Attr.href "#main-content"
            , Attr.class "sr-only focus:not-sr-only focus:absolute focus:top-4 focus:left-4 focus:z-50 focus:px-4 focus:py-2 focus:bg-blue-600 focus:text-white focus:rounded-none"
            ]
            [ text "Siirry pääsisältöön" ]
        , -- Hamburger menu button for mobile
          button
            [ Events.onClick ToggleSidebar
            , Attr.class "md:hidden fixed top-4 right-4 z-40 p-3 rounded-none text-white"
            , Attr.style "cursor" "pointer"
            , Attr.style "font-size" "2em"
            , Attr.style "padding-top" "0"
            , Attr.style "mix-blend-mode" "difference"
            , Attr.attribute "aria-label" (if model.isSidebarVisible then "Sulje valikko" else "Avaa valikko")
            ]
            [ text (if model.isSidebarVisible then "✕" else "☰") ]
        , div [ Attr.class "flex" ]
            [ -- Timeline navigation
              renderTimelineNav model.visibleGroups
            , -- Main content
              main_
                [ Attr.id "main-content"
                , Attr.class "flex-1 p-6"
                ]
                [ renderIntro
                , Html.Keyed.node "div"
                    []
                    (List.map
                        (\group -> ( group.monthId, lazy2 renderMonthSection model.viewMode group ))
                        model.visibleGroups
                    )
                , renderFooter model.generatedAt
                ]
            , -- Feed filter navigation
              renderFeedFilterNav model.selectedFeedTypes model.searchText model.viewMode
            ]
        , -- Mobile sidebar
          renderMobileSidebar model
        , -- Overlay for mobile sidebar
          if model.isSidebarVisible then
            div
                [ Attr.class "md:hidden fixed inset-0 z-30"
                , Events.onClick ToggleSidebar
                ]
                []
          else
            text ""
        , -- Scroll to top button
          if model.scrollY > 200 then
            button
                [ Events.onClick ScrollToTop
                , Attr.class "fixed bottom-4 right-4 z-50 p-3 text-white"
                , Attr.style "mix-blend-mode" "difference"
                , Attr.style "font-size" "2em"
                , Attr.style "cursor" "pointer"
                , Attr.attribute "aria-label" "Scroll to top"
                ]
                [ text "⬆️" ]
          else
            text ""
        ]


renderTimelineNav : List MonthGroup -> Html Msg
renderTimelineNav groups =
    nav [ Attr.class "hidden md:block w-48 bg-white shadow-lg p-4 sticky top-0 h-screen overflow-y-auto" ]
        [ h2 [ Attr.class "sr-only" ] [ text "Aikajana" ]
        , ul [ Attr.class "space-y-2" ]
            (List.map
                (\group ->
                    li []
                        [ a
                            [ Attr.href ("#" ++ group.monthId)
                            , Attr.class "text-sm text-gray-600 hover:text-blue-600 hover:underline"
                            ]
                            [ text group.monthLabel ]
                        ]
                )
                groups
            )
        ]


renderFeedFilterNav : List FeedType -> String -> ViewMode -> Html Msg
renderFeedFilterNav selectedFeedTypes searchText viewMode =
    nav [ Attr.class "hidden md:block w-48 bg-white shadow-lg p-4 sticky top-0 h-screen overflow-y-auto" ]
        [ h2 [ Attr.class "sr-only" ] [ text "Feed filters" ]
        , div [ Attr.class "mb-4" ]
            [ input
                [ Attr.type_ "text"
                , Attr.placeholder "Hae..."
                , Attr.value searchText
                , Events.onInput UpdateSearchText
                , Attr.class "w-full px-3 py-2 border border-gray-300 rounded-none focus:outline-none focus:ring-2 focus:ring-blue-500"
                ]
                []
            ]
        , div [ Attr.class "flex flex-wrap gap-2 mb-4" ]
            (List.map
                (\feedType ->
                    button
                        [ Events.onClick (ToggleFeedType feedType)
                        , Attr.class ("cursor-pointer text-2xl p-2 rounded-none border " ++
                            if List.member feedType selectedFeedTypes then
                                "bg-blue-100 border-blue-300 text-blue-700"
                            else
                                "bg-gray-100 border-gray-300 text-gray-500 opacity-50"
                            )
                        , Attr.title (feedTypeToString feedType)
                        , Attr.attribute "aria-label" (feedTypeToString feedType)
                        ]
                        [ text (feedTypeIcon feedType) ]
                )
                [ Feed, YouTube, Image ]
            )
        , div [ Attr.class "mb-4" ]
            [ label [ Attr.class "sr-only" ] [ text "Näkymä" ]
            , button
                [ Events.onClick (ToggleViewMode (if viewMode == Full then Thumbnail else Full))
                , Attr.class ("cursor-pointer px-3 py-1 text-sm rounded-none border w-full " ++
                    if viewMode == Full then
                        "bg-blue-100 border-blue-300 text-blue-700"
                    else
                        "bg-gray-100 border-gray-300 text-gray-500 opacity-50"
                    )
                , Attr.attribute "aria-label" "Kuvaukset"
                ]
                [ text "👁️ Kuvaukset" ]
            ]
        ]


renderMobileSidebar : Model -> Html Msg
renderMobileSidebar model =
    div
        [ Attr.class ("md:hidden fixed inset-y-0 left-0 w-64 bg-white shadow-lg z-40 transform overflow-y-auto " ++
            if model.isSidebarVisible then
                "translate-x-0"
            else
                "-translate-x-full"
            )
        ]
        [ -- Close button
          button
            [ Events.onClick ToggleSidebar
            , Attr.class "sr-only"
            , Attr.attribute "aria-label" "Sulje valikko"
            ]
            [ text "✕" ]
        , -- Feed filter navigation
          nav [ Attr.class "p-4" ]
            [ h2 [ Attr.class "sr-only" ] [ text "Suodattimet" ]
            , div [ Attr.class "mb-4" ]
                [ input
                    [ Attr.type_ "text"
                    , Attr.placeholder "Hae..."
                    , Attr.value model.searchText
                    , Events.onInput UpdateSearchText
                    , Attr.class "w-full px-3 py-2 border border-gray-300 rounded-none focus:outline-none focus:ring-2 focus:ring-blue-500"
                    ]
                    []
                ]
            , div [ Attr.class "flex flex-wrap gap-2 mb-4" ]
                (List.map
                    (\feedType ->
                        button
                            [ Events.onClick (ToggleFeedType feedType)
                            , Attr.class ("cursor-pointer text-2xl p-2 rounded-none border " ++
                                if List.member feedType model.selectedFeedTypes then
                                    "bg-blue-100 border-blue-300 text-blue-700"
                                else
                                    "bg-gray-100 border-gray-300 text-gray-500 opacity-50"
                                )
                            , Attr.title (feedTypeToString feedType)
                            , Attr.attribute "aria-label" (feedTypeToString feedType)
                            ]
                            [ text (feedTypeIcon feedType) ]
                    )
                    [ Feed, YouTube, Image ]
                )
            , div [ Attr.class "mb-4" ]
                [ label [ Attr.class "sr-only" ] [ text "Näkymä" ]
                , button
                    [ Events.onClick (ToggleViewMode (if model.viewMode == Full then Thumbnail else Full))
                    , Attr.class ("cursor-pointer px-3 py-1 text-sm rounded-none border w-full " ++
                        if model.viewMode == Full then
                            "bg-blue-100 border-blue-300 text-blue-700"
                        else
                            "bg-gray-100 border-gray-300 text-gray-500 opacity-50"
                        )
                    , Attr.attribute "aria-label" "Kuvaukset"
                    ]
                    [ text "👁️ Kuvaukset" ]
                ]
            ]
        , -- Timeline navigation
          nav [ Attr.class "p-4 border-t border-gray-300" ]
            [ h2 [ Attr.class "sr-only" ] [ text "Aikajana" ]
            , ul [ Attr.class "space-y-2" ]
                (List.map
                    (\group ->
                        li []
                            [ a
                                [ Attr.href ("#" ++ group.monthId)
                                , Attr.class "text-sm text-gray-600 hover:text-blue-600 hover:underline"
                                , Events.onClick ToggleSidebar
                                ]
                                [ text group.monthLabel ]
                            ]
                    )
                    model.visibleGroups
                )
            ]
        ]


feedTypeToString : FeedType -> String
feedTypeToString feedType =
    case feedType of
        Feed ->
            "Feed"

        YouTube ->
            "YouTube"

        Image ->
            "Image"


renderIntro : Html Msg
renderIntro =
    div [ Attr.class "mb-8" ]
        [ h1 [ Attr.class "text-3xl font-bold text-gray-800" ] [ text "Palikkalinkit" ]
        ]


renderMonthSection : ViewMode -> MonthGroup -> Html Msg
renderMonthSection viewMode group =
    div
        [ Attr.id group.monthId
        , Attr.class "mb-8"
        , Attr.style "scroll-margin-top" "80px"
        ]
        [ h2 [ Attr.class "text-xl font-semibold text-gray-700 mb-4 border-b pb-2" ]
            [ text group.monthLabel ]
        , Html.Keyed.node "div"
            [ Attr.class "grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4 gap-4" ]
            (List.map
                (\item -> ( item.itemLink, lazy2 renderCard viewMode item ))
                group.items
            )
        ]


renderCard : ViewMode -> AppItem -> Html Msg
renderCard viewMode item =
    case viewMode of
        Full ->
            renderFullCard item

        Thumbnail ->
            renderThumbnailCard item


renderFullCard : AppItem -> Html Msg
renderFullCard item =
    div [ Attr.class "bg-white rounded-none shadow-md overflow-hidden hover:shadow-lg transition-shadow" ]
        [ -- Card image
          case item.itemThumbnail of
            Just url ->
                div [ Attr.class "aspect-[4/3] bg-gray-100" ]
                    [ a [ Attr.href item.itemLink, Attr.target "_blank", Attr.rel "noopener noreferrer", Attr.attribute "aria-label" (item.itemTitle ++ " (avaa uudessa ikkunassa)") ]
                        [ img
                            [ Attr.src url
                            , Attr.alt item.itemTitle
                            , Attr.class ("w-full h-full object-cover" ++ (if item.itemType /= YouTube then " object-top" else ""))
                            ]
                            []
                        ]
                    ]

            Nothing ->
                div [ Attr.class "aspect-[4/3] bg-gray-200 flex items-center justify-center" ]
                    [ span [ Attr.class "text-4xl", Attr.attribute "aria-label" (feedTypeName item.itemType) ] [ text (feedTypeIcon item.itemType) ]
                    ]
        , -- Card content
          div [ Attr.class "p-4" ]
            [ -- Source link
              case item.itemSourceLink of
                Just url ->
                    a
                        [ Attr.href url
                        , Attr.target "_blank"
                        , Attr.rel "noopener noreferrer"
                        , Attr.attribute "aria-label" (item.itemSourceTitle ++ " (avaa uudessa ikkunassa)")
                        , Attr.class "text-xs text-blue-600 hover:underline"
                        ]
                        [ text item.itemSourceTitle ]

                Nothing ->
                    span [ Attr.class "text-xs text-gray-500" ] [ text item.itemSourceTitle ]
            , -- Title
              h3 [ Attr.class "font-semibold text-gray-800 mt-1 line-clamp-2" ]
                [ a
                    [ Attr.href item.itemLink
                    , Attr.target "_blank"
                    , Attr.rel "noopener noreferrer"
                    , Attr.attribute "aria-label" (item.itemTitle ++ " (avaa uudessa ikkunassa)")
                    , Attr.class "hover:text-blue-600"
                    ]
                    [ text item.itemTitle ]
                ]
            , -- Description (truncated)
              case item.itemDescSnippet of
                Just desc ->
                    p [ Attr.class "text-sm text-gray-600 mt-2 line-clamp-2" ]
                        [ text desc ]

                Nothing ->
                    text ""
            ]
        , -- Card meta (date and type icon)
          div [ Attr.class "px-4 pb-3 flex justify-between items-center text-xs text-gray-500" ]
            [ case item.itemDate of
                Just date ->
                    span [] [ text (formatDate date) ]

                Nothing ->
                    text ""
            , span [ Attr.class "text-lg", Attr.title (feedTypeName item.itemType), Attr.attribute "aria-label" (feedTypeName item.itemType) ]
                [ text (feedTypeIcon item.itemType) ]
            ]
        ]


renderThumbnailCard : AppItem -> Html Msg
renderThumbnailCard item =
    div [ Attr.class "bg-white rounded-none shadow-md overflow-hidden hover:shadow-lg transition-shadow" ]
        [ -- Card image only
          case item.itemThumbnail of
            Just url ->
                div [ Attr.class "aspect-[4/3] bg-gray-100" ]
                    [ a [ Attr.href item.itemLink, Attr.target "_blank", Attr.rel "noopener noreferrer", Attr.attribute "aria-label" (item.itemTitle ++ " (avaa uudessa ikkunassa)") ]
                        [ img
                            [ Attr.src url
                            , Attr.alt item.itemTitle
                            , Attr.class ("w-full h-full object-cover" ++ (if item.itemType /= YouTube then " object-top" else ""))
                            ]
                            []
                        ]
                    ]

            Nothing ->
                div [ Attr.class "aspect-[4/3] bg-gray-200 flex items-center justify-center" ]
                    [ a [ Attr.href item.itemLink, Attr.target "_blank", Attr.rel "noopener noreferrer", Attr.attribute "aria-label" (item.itemTitle ++ " (avaa uudessa ikkunassa)") ]
                        [ span [ Attr.class "text-2xl", Attr.attribute "aria-label" (feedTypeName item.itemType) ] [ text (feedTypeIcon item.itemType) ]
                        ]
                    ]
        ]


{-| Get emoji icon for feed type
-}
feedTypeIcon : FeedType -> String
feedTypeIcon feedType =
    case feedType of
        Feed ->
            "📝"

        YouTube ->
            "🎥"

        Image ->
            "📷"


{-| Get human-readable name for feed type
-}
feedTypeName : FeedType -> String
feedTypeName feedType =
    case feedType of
        Feed ->
            "Syöte"

        YouTube ->
            "YouTube-video"

        Image ->
            "Kuva"


{-| Very basic HTML tag stripping (iterative to avoid stack overflow)
-}
stripHtml : String -> String
stripHtml str =
    let
        step char ( inTag, acc ) =
            if char == '<' then
                ( True, acc )

            else if char == '>' then
                ( False, ' ' :: acc )

            else if inTag then
                ( True, acc )

            else
                ( False, char :: acc )

        ( _, reversed ) =
            String.foldl step ( False, [] ) str
    in
    reversed
        |> List.reverse
        |> String.fromList
        |> String.trim


{-| Truncate text to max length with ellipsis
-}
truncateText : Int -> String -> String
truncateText maxLen str =
    if String.length str <= maxLen then
        str
    else
        String.left maxLen str ++ "..."


renderFooter : String -> Html Msg
renderFooter timestamp =
    footer [ Attr.class "mt-12 pt-6 border-t text-center text-gray-500 text-sm" ]
        [ p [] [ text "Suomen Palikkayhteisö ry:n tuottama syötekooste" ]
        , p [ Attr.class "mt-1" ] [ text ("Koottu " ++ timestamp) ]
        ]