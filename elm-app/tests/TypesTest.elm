module TypesTest exposing (suite)

{-| Tests for Types module

Covers: Type definitions for the Elm application
Constrained by: ADR-0000-agent-guidance.md

-}

import Expect
import Test exposing (Test, describe, test)
import Types exposing (Model, MonthGroup, Msg(..))


suite : Test
suite =
    describe "Types module"
        [ describe "Model"
            [ test "Model contains items and generatedAt" <|
                \_ ->
                    let
                        model : Model
                        model =
                            { items = []
                            , generatedAt = "2026-01-09"
                            }
                    in
                    Expect.equal model.generatedAt "2026-01-09"
            ]
        , describe "MonthGroup"
            [ test "MonthGroup contains month metadata and items" <|
                \_ ->
                    let
                        group : MonthGroup
                        group =
                            { monthLabel = "tammikuu 2026"
                            , monthId = "m-2026-01"
                            , items = []
                            }
                    in
                    Expect.equal group.monthLabel "tammikuu 2026"
            ]
        , describe "Msg"
            [ test "NoOp message exists" <|
                \_ ->
                    Expect.equal NoOp NoOp
            ]
        ]
