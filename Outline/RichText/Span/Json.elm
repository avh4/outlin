module Outline.RichText.Span.Json (toJson, decoder) where

import Core.Action
import Core.Array
import Core.String
import Json.Decode
import Json.Decode ((:=))
import RichText (..)

typeString t = case t of
  Normal -> "Normal"
  Bold -> "Bold"
  -- TODO: more cases

toJson : Span -> String
toJson (t,v) = "{\"type\":\"" ++ typeString t
  ++ "\",\"value\":" ++ Core.String.toJson v
  ++ "}"

typeDecoder : Json.Decode.Decoder SpanType
typeDecoder = Json.Decode.string |> Json.Decode.map (\s -> case s of
  "Normal" -> Normal
  "Bold" -> Bold
  -- TODO: more cases
  )

decoder : Json.Decode.Decoder (SpanType, Core.String.Value)
decoder = Json.Decode.object2 (,)
  ("type" := typeDecoder)
  ("value" := Core.String.decoder)
