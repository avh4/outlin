module Outline.RichText.Json
  ( toJson
  , decoder, stringDecoder
  ) where

import Core.Action
import Core.Array
import Core.String
import Json.Encode
import Json.Decode
import Json.Decode ((:=))
import Outline.RichText.Model (..)
import Outline.RichText.Block.Model as Block
import Outline.RichText.Block.Json as Block
import List

toJson : Value -> Json.Encode.Value
toJson v = List.map Block.toJson v |> Json.Encode.list

decoder : Json.Decode.Decoder Value
decoder = Json.Decode.list Block.decoder

stringDecoder : Json.Decode.Decoder Value
stringDecoder = Json.Decode.string
  |> Json.Decode.map value
