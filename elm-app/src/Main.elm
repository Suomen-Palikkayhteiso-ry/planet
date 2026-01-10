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
import Ports
import Types exposing (Model, Msg(..), ViewMode(..))
import View


{-| Flags passed from JavaScript containing timestamp and saved view mode
-}
type alias Flags =
    { timestamp : String
    , viewMode : String
    }


{-| Parse view mode string to ViewMode, defaulting to Full
-}
parseViewMode : String -> ViewMode
parseViewMode str =
    case str of
        "Thumbnail" ->
            Thumbnail

        _ ->
            Full


{-| Convert ViewMode to string for localStorage
-}
viewModeToString : ViewMode -> String
viewModeToString viewMode =
    case viewMode of
        Full ->
            "Full"

        Thumbnail ->
            "Thumbnail"


{-| Main program entry point
-}
main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = \_ -> NoOp
        , onUrlChange = \_ -> NoOp
        }


{-| View function (delegate to View module)
-}
view : Model -> Browser.Document Msg
view model =
    { title = "Palikkalinkit"
    , body = [ View.view model ]
    }



-- MODEL


{-| Initialize the model with feed items, generation timestamp, and all feed types selected
-}
init : Flags -> url -> key -> ( Model, Cmd Msg )
init flags _ _ =
    ( { items = allAppItems
      , generatedAt = flags.timestamp
      , selectedFeedTypes = [ Feed, YouTube, Image ]
      , searchText = ""
      , viewMode = parseViewMode flags.viewMode
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

        UpdateSearchText text ->
            ( { model | searchText = text }, Cmd.none )

        ToggleViewMode viewMode ->
            ( { model | viewMode = viewMode }
            , Ports.saveViewMode (viewModeToString viewMode)
            )



-- SUBSCRIPTIONS


{-| Subscriptions (currently none)
-}
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
