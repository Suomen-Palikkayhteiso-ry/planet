module Main exposing (main, init, update, subscriptions)

{-| Main entry point for the Palikkalinkit application

This module orchestrates the Elm application, delegating to specialized modules:

  - Types: Core type definitions
  - DateUtils: Date formatting and grouping utilities
  - View: UI rendering logic
  - Data: Feed data and types

-}

import Browser
import Data exposing (allAppItems, FeedType(..))
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


{-| Initialize the model with feed items, generation timestamp, and all feed types selected
-}
init : String -> ( Model, Cmd Msg )
init timestamp =
    ( { items = allAppItems
      , generatedAt = timestamp
      , selectedFeedTypes = [ Rss, YouTube, Flickr, Atom, Kuvatfi ]
      }
    , Cmd.none
    )



-- UPDATE


{-| Update function for the Elm Architecture
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ToggleFeedType feedType ->
            let
                newSelected =
                    if List.member feedType model.selectedFeedTypes then
                        List.filter ((/=) feedType) model.selectedFeedTypes

                    else
                        feedType :: model.selectedFeedTypes
            in
            ( { model | selectedFeedTypes = newSelected }, Cmd.none )



-- SUBSCRIPTIONS


{-| Subscriptions (currently none)
-}
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
