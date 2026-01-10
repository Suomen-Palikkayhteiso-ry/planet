module Types exposing
    ( Model
    , MonthGroup
    , Msg(..)
    , ViewMode(..)
    , SearchItem
    )

{-| Core types for the application

@docs Model, MonthGroup, Msg, ViewMode, SearchItem

-}

import Data exposing (AppItem)
import Http
import RemoteData exposing (RemoteData)


{-| View mode for displaying items
-}
type ViewMode
    = Full
    | Thumbnail


{-| Search item for client-side search
-}
type alias SearchItem =
    { id : String
    , title : String
    , description : String
    , source : String
    }


{-| The application model containing all feed items, generation timestamp, selected feed types, search text, and view mode
-}
type alias Model =
    { items : List AppItem
    , generatedAt : String
    , selectedFeedTypes : List Data.FeedType
    , searchText : String
    , viewMode : ViewMode
    , visibleGroups : List MonthGroup
    , isSidebarVisible : Bool
    , searchIndex : RemoteData Http.Error (List SearchItem)
    , searchedIds : List Int
    , scrollY : Float
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
    | ToggleSidebar
    | OnSearchIndexFetch (Result Http.Error (List SearchItem))
    | OnSearchResults (List Int)
    | LoadViewMode String
    | LoadSelectedFeedTypes String
    | ScrollY Float
    | ScrollToTop
