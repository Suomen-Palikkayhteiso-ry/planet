module Types exposing
    ( Model
    , MonthGroup
    , Msg(..)
    )

{-| Core types for the application

@docs Model, MonthGroup, Msg

-}

import Data exposing (AppItem)


{-| The application model containing all feed items, generation timestamp, and selected feed types
-}
type alias Model =
    { items : List AppItem
    , generatedAt : String
    , selectedFeedTypes : List Data.FeedType
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
    | ToggleFeedType Data.FeedType
