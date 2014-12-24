module Outline.Notes.Json
  ( toJson
  , decoder
  ) where

import Core.Action
import Core.Array
import Core.String
import Outline.Notes.Model (..)
import Json.Decode
import Outline.RichText.Json as RichText

toJson : Value -> String
toJson = Core.Array.toJson RichText.toJson

decoder : Json.Decode.Decoder Value
decoder = Core.Array.decoder RichText.decoder
