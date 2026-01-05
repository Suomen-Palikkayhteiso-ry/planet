{-# LANGUAGE OverloadedStrings #-}

module ElmGen (generateElmModule) where

import Data.Aeson (ToJSON (..), Value (..))
import Data.Aeson.Key (toText)
import Data.Aeson.KeyMap (toList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import I18n (AppItem)

generateElmModule :: [AppItem] -> Text
generateElmModule items =
    let value = toJSON items
        elmCode = jsonToElm (T.pack "allAppItems") value
     in T.unlines
            [ T.pack "module Data exposing (allAppItems)"
            , T.pack ""
            , T.pack "import Maybe exposing (Maybe(..))"
            , T.pack "import Data exposing (FeedType(..), AppItem)"
            , T.pack ""
            , elmCode
            ]

jsonToElm :: Text -> Value -> Text
jsonToElm name value =
    T.concat [name, T.pack " : List AppItem\n", name, T.pack " =\n", valueToElm value, T.pack "\n"]

valueToElm :: Value -> Text
valueToElm (Object obj) =
    let fields = map (\(k, v) -> T.concat [T.pack "        ", toText k, T.pack " = ", valueToElm' (toText k) v]) (toList obj)
     in T.unlines [T.pack "    {", T.intercalate (T.pack ",\n") fields, T.pack "    }"]
valueToElm (Array arr) =
    let values = map valueToElm (V.toList arr)
     in T.unlines [T.pack "[", T.intercalate (T.pack ",\n") values, T.pack "]"]
valueToElm (String s) = T.concat [T.pack "\"", s, T.pack "\""]
valueToElm (Number n) = T.pack (show n)
valueToElm (Bool b) = if b then T.pack "True" else T.pack "False"
valueToElm Null = T.pack "Nothing"

valueToElm' :: Text -> Value -> Text
valueToElm' "itemType" (String s) = s
valueToElm' "date" Null = T.pack "Nothing"
valueToElm' "date" (String s) = T.concat [T.pack "Just \"", s, T.pack "\""]
valueToElm' "description" Null = T.pack "Nothing"
valueToElm' "description" (String s) = T.concat [T.pack "Just \"", s, T.pack "\""]
valueToElm' "thumbnail" Null = T.pack "Nothing"
valueToElm' "thumbnail" (String s) = T.concat [T.pack "Just \"", s, T.pack "\""]
valueToElm' "sourceLink" Null = T.pack "Nothing"
valueToElm' "sourceLink" (String s) = T.concat [T.pack "Just \"", s, T.pack "\""]
valueToElm' _ v = valueToElm v
