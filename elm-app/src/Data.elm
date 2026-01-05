module Data exposing (AppItem, FeedType(..))


type FeedType
    = YouTube
    | Rss
    | Flickr
    | Kuvatfi
    | Atom


type alias AppItem =
    { title : String
    , link : String
    , date : Maybe String
    , description : Maybe String
    , thumbnail : Maybe String
    , sourceTitle : String
    , sourceLink : Maybe String
    , itemType : FeedType
    }
