module Outline.Scratch.Json (toJson, decoder, listDecoder) where

import Core.Action
import Core.Array
import Core.String
import Outline.Scratch.Model (..)
import Json.Decode
import Outline.RichText.Json as RichText

toJson : Value -> String
toJson = RichText.toJson

decoder : Json.Decode.Decoder Value
decoder = RichText.decoder

listDecoder : Json.Decode.Decoder (Core.Array.Value Value)
listDecoder = Json.Decode.list decoder
