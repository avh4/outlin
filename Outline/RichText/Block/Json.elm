module Outline.RichText.Block.Json (toJson, decoder) where

import Core.Action
import Core.Array
import Core.String
import Json.Encode
import Json.Encode as Json
import Json.Decode
import Json.Decode ((:=))
import Outline.RichText.Block.Model (..)
import RichText.Json (..)
import List

typeString t = case t of
  Heading -> "Heading"
  Paragraph -> "Paragraph"
  Quote -> "Quote"
  Task -> "Task"

toJson' : Value -> Json.Value
toJson' (t,v) = Json.Encode.object
  [ ("type", Json.Encode.string <| typeString t)
  , ("value", List.map encodeSpan v |> Json.Encode.list)
  ]

toJson : Value -> String
toJson v = toJson' v
  |> Json.Encode.encode 0

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
  ("value" := Json.Decode.list spanDecoder)
