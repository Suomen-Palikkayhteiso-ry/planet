module Types exposing
    ( Model
    , MonthGroup
    , Msg(..)
    )

{-| Core types for the application

@docs Model, MonthGroup, Msg

-}

import Data exposing (AppItem)


{-| The application model containing all feed items and generation timestamp
-}
type alias Model =
    { items : List AppItem
    , generatedAt : String
    }


{-| A group of items for a specific month
-}
type alias MonthGroup =
    { monthLabel : String
    , monthId : String
    , items : List AppItem
    }


{-| Messages for the Elm Architecture update loop
-}
type Msg
    = NoOp
