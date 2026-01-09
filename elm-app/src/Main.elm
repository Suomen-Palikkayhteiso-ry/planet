module Main exposing (main, init, update, subscriptions)

{-| Main entry point for the Palikkalinkit application

This module orchestrates the Elm application, delegating to specialized modules:

  - Types: Core type definitions
  - DateUtils: Date formatting and grouping utilities
  - View: UI rendering logic
  - Data: Feed data and types

-}

import Browser
import Data exposing (allAppItems)
import Html exposing (Html)
import Types exposing (Model, Msg(..))
import View


{-| Main program entry point
-}
main : Program String Model Msg
main =
    Browser.element
        { init = init
        , view = View.view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


{-| Initialize the model with feed items and generation timestamp
-}
init : String -> ( Model, Cmd Msg )
init timestamp =
    ( { items = allAppItems, generatedAt = timestamp }, Cmd.none )



-- UPDATE


{-| Update function for the Elm Architecture
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


{-| Subscriptions (currently none)
-}
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
