module Outline.RichText.Block.Json (toJson, decoder) where

import Core.Action
import Core.Array
import Core.String
import Json.Decode
import Json.Decode ((:=))
import Outline.RichText.Block.Model (..)
import Outline.RichText.Span.Model as Span
import Outline.RichText.Span.Json as Span

typeString t = case t of
  Heading -> "Heading"
  Paragraph -> "Paragraph"
  Quote -> "Quote"
  Task -> "Task"

toJson : Value -> String
toJson (t,v) = "{\"type\":\"" ++ typeString t
  ++ "\",\"value\":" ++ Core.Array.toJson Span.toJson v
  ++ "}"

typeDecoder : Json.Decode.Decoder Type
typeDecoder = Json.Decode.string |> Json.Decode.map (\s -> case s of
  "Heading" -> Heading
  "Paragraph" -> Paragraph
  "Quote" -> Quote
  "Task" -> Task
  )

decoder : Json.Decode.Decoder Value
decoder = Json.Decode.object2 (,)
  ("type" := typeDecoder)
  ("value" := Core.Array.decoder Span.decoder)
