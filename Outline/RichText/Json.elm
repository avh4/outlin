module Outline.RichText.Json (toJson, decoder) where

import Core.Action
import Core.Array
import Core.String
import Json.Decode
import Json.Decode ((:=))
import Outline.RichText.Model (..)
import Outline.RichText.Span.Model as Span
import Outline.RichText.Span.Json as Span

toJson : Value -> String
toJson = Core.Array.toJson Span.toJson

decoder : Json.Decode.Decoder Value
decoder = Core.Array.decoder Span.decoder
