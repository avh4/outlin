module Outline.RichText.Json (toJson, decoder) where

import Core.Action
import Core.Array
import Core.String
import Json.Decode
import Json.Decode ((:=))
import Outline.RichText.Model (..)
import Outline.RichText.Block.Model as Block
import Outline.RichText.Block.Json as Block

toJson : Value -> String
toJson = Core.Array.toJson Block.toJson

decoder : Json.Decode.Decoder Value
decoder = Core.Array.decoder Block.decoder
