module Outline.Document.Json (toJson) where

import Outline.Document.Model (..)
import Core.Array
import Outline.Entry as Entry
import Outline.Scratch.Json as Scratch
import Json.Encode
import List

-- TODO: no one uses this...
toJson : Value -> Json.Encode.Value
toJson v = Json.Encode.object
  [ ("scratch", List.map Scratch.toJson v.scratch |> Json.Encode.list)
  , ("outline", Entry.toJson v.tasks)
  ]
