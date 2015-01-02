module RichText.Json.Decode
  ( span, spanType
  ) where

import Json.Decode (..)
import RichText (..)

spanTypeFromString : String -> Result String SpanType
spanTypeFromString s = case s of
  "Normal" -> Ok Normal
  "Bold" -> Ok Bold
  -- TODO: more cases
  _ -> Err ("Invalid span type: " ++ s)

spanType : Decoder SpanType
spanType = customDecoder string spanTypeFromString

span : Decoder Span
span = object2 (,)
  ("type" := spanType)
  ("value" := string)
