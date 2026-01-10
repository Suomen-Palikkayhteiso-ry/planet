port module Ports exposing (saveViewMode, loadViewMode)

{-| Ports for communicating with JavaScript for localStorage persistence
-}


{-| Send view mode to JavaScript to save in localStorage
-}
port saveViewMode : String -> Cmd msg


{-| Receive view mode from JavaScript on app initialization
-}
port loadViewMode : (String -> msg) -> Sub msg