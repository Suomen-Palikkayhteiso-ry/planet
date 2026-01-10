module MainTest exposing (suite)

{-| Tests for Main module

Covers: US-001 (Aggregate Feeds) - Application initialization and orchestration
Constrained by: ADR-0000-agent-guidance.md

-}

import Data exposing (allAppItems, FeedType(..))
import Expect
import Main
import Test exposing (Test, describe, test)
import Types exposing (Msg(..))


suite : Test
suite =
    describe "Main module"
        [ describe "init"
            [ test "initializes model with items and timestamp" <|
                \_ ->
                    let
                        ( model, _ ) =
                            Main.init { timestamp = "2026-01-09", viewMode = "Full" } "url" "key"
                    in
                    Expect.equal model.generatedAt "2026-01-09"
            , test "initializes with all app items" <|
                \_ ->
                    let
                        ( model, _ ) =
                            Main.init { timestamp = "2026-01-09", viewMode = "Full" } "url" "key"
                    in
                    Expect.equal model.items allAppItems
            , test "initializes with all feed types selected" <|
                \_ ->
                    let
                        ( model, _ ) =
                            Main.init { timestamp = "2026-01-09", viewMode = "Full" } "url" "key"
                    in
                    Expect.equal model.selectedFeedTypes [ Feed, YouTube, Image ]
            , test "initializes with empty search text" <|
                \_ ->
                    let
                        ( model, _ ) =
                            Main.init { timestamp = "2026-01-09", viewMode = "Full" } "url" "key"
                    in
                    Expect.equal model.searchText ""
            ]
        , describe "update"
            [ test "NoOp returns unchanged model" <|
                \_ ->
                    let
                        ( initialModel, _ ) =
                            Main.init { timestamp = "2026-01-09", viewMode = "Full" } "url" "key"

                        ( updatedModel, _ ) =
                            Main.update NoOp initialModel
                    in
                    Expect.equal initialModel updatedModel
            , test "ToggleFeedType toggles the feed type in selectedFeedTypes" <|
                \_ ->
                    let
                        ( initialModel, _ ) =
                            Main.init { timestamp = "2026-01-09", viewMode = "Full" } "url" "key"

                        ( updatedModel, _ ) =
                            Main.update (ToggleFeedType Feed) initialModel
                    in
                    Expect.equal (List.member Feed updatedModel.selectedFeedTypes) False
            , test "UpdateSearchText updates the search text" <|
                \_ ->
                    let
                        ( initialModel, _ ) =
                            Main.init { timestamp = "2026-01-09", viewMode = "Full" } "url" "key"

                        ( updatedModel, _ ) =
                            Main.update (UpdateSearchText "test search") initialModel
                    in
                    Expect.equal updatedModel.searchText "test search"
            ]
        , describe "subscriptions"
            [ test "returns Sub.none" <|
                \_ ->
                    let
                        ( model, _ ) =
                            Main.init { timestamp = "2026-01-09", viewMode = "Full" } "url" "key"
                    in
                    Expect.equal Sub.none (Main.subscriptions model)
            ]
        ]
