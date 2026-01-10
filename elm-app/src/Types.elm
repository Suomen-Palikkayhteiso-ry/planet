module Types exposing
    ( Model
    , MonthGroup
    , Msg(..)
    , ViewMode(..)
    )

{-| Core types for the application

@docs Model, MonthGroup, Msg, ViewMode

-}

import Data exposing (AppItem)


{-| View mode for displaying items
-}
type ViewMode
    = Full
    | Thumbnail


{-| The application model containing all feed items, generation timestamp, selected feed types, search text, and view mode
-}
type alias Model =
    { items : List AppItem
    , generatedAt : String
    , selectedFeedTypes : List Data.FeedType
    , searchText : String
    , viewMode : ViewMode
    , visibleGroups : List MonthGroup
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
    | UpdateSearchText String
    | ApplySearch
    | ToggleViewMode ViewMode
