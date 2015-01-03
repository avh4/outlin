module Outline.RichText.Json
  ( toJson
  , decoder
  ) where

import Json.Encode
import Json.Decode
import Outline.RichText.Model (..)
import RichText
import RichText.Json.Encode as Encode
import RichText.Json.Decode as Decode
import String
import List

toJson : Value -> Json.Encode.Value
toJson v = List.map Encode.block v |> Json.Encode.list

decoderV1 : Json.Decode.Decoder Value
decoderV1 = Json.Decode.string
  |> Json.Decode.map (String.split "\n" >> List.map RichText.paragraph)

decoderV2 : Json.Decode.Decoder Value
decoderV2 = Json.Decode.list Decode.block

decoder : Json.Decode.Decoder Value
decoder = Json.Decode.oneOf
  [ decoderV2
  , decoderV1
  ]
