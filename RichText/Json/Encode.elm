module RichText.Json.Encode
  ( span, spanType
  ) where

import Json.Encode (..)
import RichText (..)

spanType : SpanType -> Value
spanType t = case t of
  Normal -> string "Normal"
  Bold -> string "Bold"
  -- Debug.crash TODO: more cases

span : Span -> Value
span (t,v) = object
  [ ("type", spanType t)
  , ("value", string v)
  ]
