module Outline.Scratch.Json (toJson, decoder, listDecoder) where

import Core.Action
import Core.Array
import Core.String
import Outline.Scratch.Model (..)
import Json.Decode

toJson = Core.String.toJson

decoder : Json.Decode.Decoder Value
decoder = Core.String.decoder

listDecoder : Json.Decode.Decoder (Core.Array.Value Value)
listDecoder =
  Json.Decode.list decoder
