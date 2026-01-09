module View exposing (view)

{-| View rendering for the application

@docs view

-}

import Data exposing (AppItem, FeedType(..))
import DateUtils exposing (formatDate, groupByMonth)
import Html exposing (Html, a, div, footer, h1, h2, h3, img, li, main_, nav, p, span, text, ul)
import Html.Attributes as Attr
import Types exposing (Model, MonthGroup, Msg(..))


{-| Main view function
-}
view : Model -> Html Msg
view model =
    let
        groups =
            groupByMonth model.items
    in
    div [ Attr.class "min-h-screen bg-gray-50" ]
        [ -- Skip to content link for accessibility
          a
            [ Attr.href "#main-content"
            , Attr.class "sr-only focus:not-sr-only focus:absolute focus:top-4 focus:left-4 focus:z-50 focus:px-4 focus:py-2 focus:bg-blue-600 focus:text-white focus:rounded"
            ]
            [ text "Siirry pääsisältöön" ]
        , div [ Attr.class "flex" ]
            [ -- Timeline navigation
              renderTimelineNav groups
            , -- Main content
              main_
                [ Attr.id "main-content"
                , Attr.class "flex-1 p-6"
                ]
                [ renderIntro
                , div [] (List.map renderMonthSection groups)
                , renderFooter model.generatedAt
                ]
            ]
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


renderIntro : Html Msg
renderIntro =
    div [ Attr.class "mb-8" ]
        [ h1 [ Attr.class "text-3xl font-bold text-gray-800" ] [ text "Palikkalinkit" ]
        ]


renderMonthSection : MonthGroup -> Html Msg
renderMonthSection group =
    div
        [ Attr.id group.monthId
        , Attr.class "mb-8"
        ]
        [ h2 [ Attr.class "text-xl font-semibold text-gray-700 mb-4 border-b pb-2" ]
            [ text group.monthLabel ]
        , div [ Attr.class "grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4 gap-4" ]
            (List.map renderCard group.items)
        ]


renderCard : AppItem -> Html Msg
renderCard item =
    div [ Attr.class "bg-white rounded-lg shadow-md overflow-hidden hover:shadow-lg transition-shadow" ]
        [ -- Card image
          case item.itemThumbnail of
            Just url ->
                div [ Attr.class "aspect-video bg-gray-100" ]
                    [ a [ Attr.href item.itemLink, Attr.target "_blank" ]
                        [ img
                            [ Attr.src url
                            , Attr.alt item.itemTitle
                            , Attr.class "w-full h-full object-cover"
                            ]
                            []
                        ]
                    ]

            Nothing ->
                div [ Attr.class "aspect-video bg-gray-200 flex items-center justify-center" ]
                    [ span [ Attr.class "text-4xl" ] [ text (feedTypeIcon item.itemType) ]
                    ]
        , -- Card content
          div [ Attr.class "p-4" ]
            [ -- Source link
              case item.itemSourceLink of
                Just url ->
                    a
                        [ Attr.href url
                        , Attr.target "_blank"
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
                    , Attr.class "hover:text-blue-600"
                    ]
                    [ text item.itemTitle ]
                ]
            , -- Description (truncated)
              case item.itemDesc of
                Just desc ->
                    p [ Attr.class "text-sm text-gray-600 mt-2 line-clamp-2" ]
                        [ text (truncateText 120 (stripHtml desc)) ]

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
            , span [ Attr.class "text-lg", Attr.title (feedTypeName item.itemType) ]
                [ text (feedTypeIcon item.itemType) ]
            ]
        ]


{-| Get emoji icon for feed type
-}
feedTypeIcon : FeedType -> String
feedTypeIcon feedType =
    case feedType of
        YouTube ->
            "🎥"

        Rss ->
            "📝"

        Flickr ->
            "📷"

        Kuvatfi ->
            "📷"

        Atom ->
            "📝"


{-| Get human-readable name for feed type
-}
feedTypeName : FeedType -> String
feedTypeName feedType =
    case feedType of
        YouTube ->
            "YouTube-video"

        Rss ->
            "RSS-syöte"

        Flickr ->
            "Flickr-kuva"

        Kuvatfi ->
            "Kuvat.fi-kuva"

        Atom ->
            "Atom-syöte"


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
