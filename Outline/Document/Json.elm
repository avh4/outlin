module Outline.Document.Json (toJson) where

import Outline.Document.Model (..)
import Core.Array
import Outline.Entry as Entry
import Outline.Scratch.Json as Scratch

toJson : Value -> String
toJson v =
  "{\"scratch\":" ++ (Core.Array.toJson Scratch.toJson v.scratch)
  ++ ",\"outline\":" ++ (Entry.toJson v.outline)
  ++ "}"
