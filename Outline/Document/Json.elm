module Outline.Document.Json (toJson) where

import Outline.Document.Model (..)
import Core.Array
import Outline.Entry as Entry
import Outline.Scratch.Json as Scratch
import Outline.Scratch.Model as Scratch
import Json.Encode
import List

-- TODO: no one uses this...
toJson : Document -> Json.Encode.Value
toJson v = Json.Encode.object
  [ ("scratch", List.map Scratch.toJson (Core.Array.toValue Scratch.toValue v.scratch) |> Json.Encode.list)
  , ("outline", Entry.toJson (Entry.toValue v.tasks))
  ]
