port module Ports exposing (saveViewMode, loadViewMode, saveSelectedFeedTypes, loadSelectedFeedTypes)

{-| Ports for communicating with JavaScript for localStorage persistence
-}


{-| Send view mode to JavaScript to save in localStorage
-}
port saveViewMode : String -> Cmd msg


{-| Receive view mode from JavaScript on app initialization
-}
port loadViewMode : (String -> msg) -> Sub msg


{-| Send selected feed types to JavaScript to save in localStorage
-}
port saveSelectedFeedTypes : String -> Cmd msg


{-| Receive selected feed types from JavaScript on app initialization
-}
port loadSelectedFeedTypes : (String -> msg) -> Sub msg