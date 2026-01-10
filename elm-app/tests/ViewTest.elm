module ViewTest exposing (suite)

{-| Tests for View module

Covers: US-005 (Self-Contained Output), US-006 (Differentiate Feed Types), US-007 (Interactive Viewer)
Constrained by: ADR-0000-agent-guidance.md

-}

import Data exposing (AppItem, FeedType(..))
import Expect
import Html exposing (Html)
import Html.Attributes
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Types exposing (Model, Msg(..))
import View


suite : Test
suite =
    describe "View module"
        [ describe "view function"
            [ test "renders main container" <|
                \_ ->
                    let
                        model =
                            { items = []
                            , generatedAt = "2026-01-09"
                            , selectedFeedTypes = [ Feed, YouTube, Image ]
                            , searchText = ""
                            }
                    in
                    View.view model
                        |> Query.fromHtml
                        |> Query.has [ Selector.class "min-h-screen" ]
            , test "renders skip to content link" <|
                \_ ->
                    let
                        model =
                            { items = []
                            , generatedAt = "2026-01-09"
                            , selectedFeedTypes = [ Feed, YouTube, Image ]
                            , searchText = ""
                            }
                    in
                    View.view model
                        |> Query.fromHtml
                        |> Query.has [ Selector.attribute (Html.Attributes.href "#main-content") ]
            , test "renders main content area" <|
                \_ ->
                    let
                        model =
                            { items = []
                            , generatedAt = "2026-01-09"
                            , selectedFeedTypes = [ Feed, YouTube, Image ]
                            , searchText = ""
                            }
                    in
                    View.view model
                        |> Query.fromHtml
                        |> Query.has [ Selector.id "main-content" ]
            , test "renders footer with generation timestamp" <|
                \_ ->
                    let
                        model =
                            { items = []
                            , generatedAt = "2026-01-09"
                            , selectedFeedTypes = [ Feed, YouTube, Image ]
                            , searchText = ""
                            }
                    in
                    View.view model
                        |> Query.fromHtml
                        |> Query.find [ Selector.tag "footer" ]
                        |> Query.has [ Selector.text "Koottu 2026-01-09" ]
            , test "renders title" <|
                \_ ->
                    let
                        model =
                            { items = []
                            , generatedAt = "2026-01-09"
                            , selectedFeedTypes = [ Feed, YouTube, Image ]
                            , searchText = ""
                            }
                    in
                    View.view model
                        |> Query.fromHtml
                        |> Query.has [ Selector.text "Palikkalinkit" ]
            , test "renders items in cards" <|
                \_ ->
                    let
                        model =
                            { items =
                                [ { itemTitle = "Test Item"
                                  , itemLink = "https://example.com"
                                  , itemDate = Just "2026-01-08T19:27:04Z"
                                  , itemDesc = Just "Test description"
                                  , itemThumbnail = Nothing
                                  , itemSourceTitle = "Test Source"
                                  , itemSourceLink = Nothing
                                  , itemType = Feed
                                  }
                                ]
                            , generatedAt = "2026-01-09"
                            , selectedFeedTypes = [ Feed, YouTube, Image ]
                            , searchText = ""
                            }
                    in
                    View.view model
                        |> Query.fromHtml
                        |> Query.has [ Selector.text "Test Item" ]
            , test "renders navigation for multiple months" <|
                \_ ->
                    let
                        model =
                            { items =
                                [ createTestItem "2026-01-08T19:27:04Z" "Item 1"
                                , createTestItem "2025-12-25T10:00:00Z" "Item 2"
                                ]
                            , generatedAt = "2026-01-09"
                            , selectedFeedTypes = [ Feed, YouTube, Image ]
                            , searchText = ""
                            }
                    in
                    View.view model
                        |> Query.fromHtml
                        |> Query.findAll [ Selector.tag "nav" ]
                        |> Query.index 0
                        |> Query.has [ Selector.text "tammikuu 2026" ]
            , test "filters items based on selected feed types - shows selected" <|
                \_ ->
                    let
                        model =
                            { items =
                                [ { itemTitle = "RSS Item"
                                  , itemLink = "https://example.com/rss"
                                  , itemDate = Just "2026-01-08T19:27:04Z"
                                  , itemDesc = Just "RSS description"
                                  , itemThumbnail = Nothing
                                  , itemSourceTitle = "RSS Source"
                                  , itemSourceLink = Nothing
                                  , itemType = Feed
                                  }
                                , { itemTitle = "YouTube Item"
                                  , itemLink = "https://example.com/yt"
                                  , itemDate = Just "2026-01-08T19:27:04Z"
                                  , itemDesc = Just "YouTube description"
                                  , itemThumbnail = Nothing
                                  , itemSourceTitle = "YouTube Source"
                                  , itemSourceLink = Nothing
                                  , itemType = YouTube
                                  }
                                ]
                            , generatedAt = "2026-01-09"
                            , selectedFeedTypes = [ Feed ] -- Only Feed selected
                            , searchText = ""
                            }
                    in
                    View.view model
                        |> Query.fromHtml
                        |> Query.has [ Selector.text "RSS Item" ]
            , test "filters items based on selected feed types - hides unselected" <|
                \_ ->
                    let
                        model =
                            { items =
                                [ { itemTitle = "RSS Item"
                                  , itemLink = "https://example.com/rss"
                                  , itemDate = Just "2026-01-08T19:27:04Z"
                                  , itemDesc = Just "RSS description"
                                  , itemThumbnail = Nothing
                                  , itemSourceTitle = "RSS Source"
                                  , itemSourceLink = Nothing
                                  , itemType = Feed
                                  }
                                , { itemTitle = "YouTube Item"
                                  , itemLink = "https://example.com/yt"
                                  , itemDate = Just "2026-01-08T19:27:04Z"
                                  , itemDesc = Just "YouTube description"
                                  , itemThumbnail = Nothing
                                  , itemSourceTitle = "YouTube Source"
                                  , itemSourceLink = Nothing
                                  , itemType = YouTube
                                  }
                                ]
                            , generatedAt = "2026-01-09"
                            , selectedFeedTypes = [ Feed ] -- Only Feed selected
                            , searchText = ""
                            }
                    in
                    View.view model
                        |> Query.fromHtml
                        |> Query.hasNot [ Selector.text "YouTube Item" ]
            , test "renders search input in feed filter nav" <|
                \_ ->
                    let
                        model =
                            { items = []
                            , generatedAt = "2026-01-09"
                            , selectedFeedTypes = [ Feed, YouTube, Image ]
                            , searchText = ""
                            }
                    in
                    View.view model
                        |> Query.fromHtml
                        |> Query.has [ Selector.attribute (Html.Attributes.placeholder "Hae...") ]
            , test "filters items based on search text - matches title" <|
                \_ ->
                    let
                        model =
                            { items =
                                [ { itemTitle = "Unique Title"
                                  , itemLink = "https://example.com"
                                  , itemDate = Just "2026-01-08T19:27:04Z"
                                  , itemDesc = Just "Some description"
                                  , itemThumbnail = Nothing
                                  , itemSourceTitle = "Source"
                                  , itemSourceLink = Nothing
                                  , itemType = Feed
                                  }
                                , { itemTitle = "Other Item"
                                  , itemLink = "https://example.com/other"
                                  , itemDate = Just "2026-01-08T19:27:04Z"
                                  , itemDesc = Just "Other description"
                                  , itemThumbnail = Nothing
                                  , itemSourceTitle = "Other Source"
                                  , itemSourceLink = Nothing
                                  , itemType = Feed
                                  }
                                ]
                            , generatedAt = "2026-01-09"
                            , selectedFeedTypes = [ Feed ]
                            , searchText = "unique"
                            }
                    in
                    View.view model
                        |> Query.fromHtml
                        |> Query.has [ Selector.text "Unique Title" ]
            , test "filters items based on search text - hides non-matches" <|
                \_ ->
                    let
                        model =
                            { items =
                                [ { itemTitle = "Unique Title"
                                  , itemLink = "https://example.com"
                                  , itemDate = Just "2026-01-08T19:27:04Z"
                                  , itemDesc = Just "Some description"
                                  , itemThumbnail = Nothing
                                  , itemSourceTitle = "Source"
                                  , itemSourceLink = Nothing
                                  , itemType = Feed
                                  }
                                , { itemTitle = "Other Item"
                                  , itemLink = "https://example.com/other"
                                  , itemDate = Just "2026-01-08T19:27:04Z"
                                  , itemDesc = Just "Other description"
                                  , itemThumbnail = Nothing
                                  , itemSourceTitle = "Other Source"
                                  , itemSourceLink = Nothing
                                  , itemType = Feed
                                  }
                                ]
                            , generatedAt = "2026-01-09"
                            , selectedFeedTypes = [ Feed ]
                            , searchText = "unique"
                            }
                    in
                    View.view model
                        |> Query.fromHtml
                        |> Query.hasNot [ Selector.text "Other Item" ]
            , test "renders feed type icons in filter nav" <|
                \_ ->
                    let
                        model =
                            { items = []
                            , generatedAt = "2026-01-09"
                            , selectedFeedTypes = [ Feed, YouTube, Image ]
                            , searchText = ""
                            }
                    in
                    View.view model
                        |> Query.fromHtml
                        |> Query.has [ Selector.tag "button" ]
            ]
        ]


{-| Helper to create a test item
-}
createTestItem : String -> String -> AppItem
createTestItem date title =
    { itemTitle = title
    , itemLink = "https://example.com"
    , itemDate = Just date
    , itemDesc = Just "Test description"
    , itemThumbnail = Nothing
    , itemSourceTitle = "Test Source"
    , itemSourceLink = Nothing
    , itemType = Feed
    }
