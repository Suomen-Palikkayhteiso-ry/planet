module MainTest exposing (suite)

{-| Tests for Main module

Covers: US-001 (Aggregate Feeds) - Application initialization and orchestration
Constrained by: ADR-0000-agent-guidance.md

-}

import Data exposing (allAppItems)
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
                            Main.init "2026-01-09"
                    in
                    Expect.equal model.generatedAt "2026-01-09"
            , test "initializes with all app items" <|
                \_ ->
                    let
                        ( model, _ ) =
                            Main.init "2026-01-09"
                    in
                    Expect.equal model.items allAppItems
            ]
        , describe "update"
            [ test "NoOp returns unchanged model" <|
                \_ ->
                    let
                        ( initialModel, _ ) =
                            Main.init "2026-01-09"

                        ( updatedModel, _ ) =
                            Main.update NoOp initialModel
                    in
                    Expect.equal initialModel updatedModel
            ]
        , describe "subscriptions"
            [ test "returns Sub.none" <|
                \_ ->
                    let
                        ( model, _ ) =
                            Main.init "2026-01-09"
                    in
                    Expect.equal Sub.none (Main.subscriptions model)
            ]
        ]
