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
import Json.Decode as Decode
import Json.Encode as Encode
import Ports
import Types exposing (Model, Msg(..), ViewMode(..))
import View


{-| Flags passed from JavaScript containing timestamp, saved view mode, and saved selected feed types
-}
type alias Flags =
    { timestamp : String
    , viewMode : String
    , selectedFeedTypes : String
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


{-| Convert FeedType to string
-}
feedTypeToString : FeedType -> String
feedTypeToString feedType =
    case feedType of
        Feed ->
            "Feed"

        YouTube ->
            "YouTube"

        Image ->
            "Image"


{-| Parse string to FeedType, defaulting to Feed
-}
stringToFeedType : String -> FeedType
stringToFeedType str =
    case str of
        "YouTube" ->
            YouTube

        "Image" ->
            Image

        _ ->
            Feed


{-| Encode list of FeedType to JSON string
-}
encodeSelectedFeedTypes : List FeedType -> String
encodeSelectedFeedTypes feedTypes =
    feedTypes
        |> List.map feedTypeToString
        |> Encode.list Encode.string
        |> Encode.encode 0


{-| Decode JSON string to list of FeedType
-}
decodeSelectedFeedTypes : String -> List FeedType
decodeSelectedFeedTypes str =
    case Decode.decodeString (Decode.list Decode.string) str of
        Ok strings ->
            List.map stringToFeedType strings

        Err _ ->
            [ Feed, YouTube, Image ] -- default


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
      , selectedFeedTypes = decodeSelectedFeedTypes flags.selectedFeedTypes
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
            ( { model | selectedFeedTypes = newSelected }
            , Ports.saveSelectedFeedTypes (encodeSelectedFeedTypes newSelected)
            )

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
