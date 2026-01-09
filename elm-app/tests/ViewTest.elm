module ViewTest exposing (suite)

{-| Tests for View module

Covers: US-005 (Self-Contained Output), US-006 (Differentiate Feed Types)
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
                                  , itemType = Rss
                                  }
                                ]
                            , generatedAt = "2026-01-09"
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
                            }
                    in
                    View.view model
                        |> Query.fromHtml
                        |> Query.find [ Selector.tag "nav" ]
                        |> Query.has [ Selector.text "tammikuu 2026" ]
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
    , itemType = Rss
    }
