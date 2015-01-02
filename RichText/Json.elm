module RichText.Json
  ( encodeSpan
  , spanDecoder
  ) where

import Core.Action
import Core.Array
import Core.String
import Json.Encode
import Json.Encode as Json
import Json.Decode
import Json.Decode ((:=))
import RichText (..)

encodeSpanType : SpanType -> Json.Value
encodeSpanType t = case t of
  Normal -> Json.Encode.string "Normal"
  Bold -> Json.Encode.string "Bold"
  -- Debug.crash TODO: more cases

encodeSpan : Span -> Json.Value
encodeSpan (t,v) = Json.Encode.object
  [ ("type", encodeSpanType t)
  , ("value", Json.Encode.string v)
  ]

spanTypeFromString : String -> Result String SpanType
spanTypeFromString s = case s of
  "Normal" -> Ok Normal
  "Bold" -> Ok Bold
  -- TODO: more cases
  _ -> Err ("Invalid span type: " ++ s)

spanTypeDecoder : Json.Decode.Decoder SpanType
spanTypeDecoder = Json.Decode.customDecoder Json.Decode.string spanTypeFromString

spanDecoder : Json.Decode.Decoder Span
spanDecoder = Json.Decode.object2 (,)
  ("type" := spanTypeDecoder)
  ("value" := Core.String.decoder)
