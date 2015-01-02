module Outline.Notes.Json
  ( toJson
  , decoder
  ) where

import Outline.Notes.Model (..)
import Json.Encode
import Json.Decode
import Outline.RichText.Json as RichText
import List

toJson : Value -> Json.Encode.Value
toJson v = List.map RichText.toJson v |> Json.Encode.list

decoder : Json.Decode.Decoder Value
decoder = Json.Decode.list RichText.decoder
