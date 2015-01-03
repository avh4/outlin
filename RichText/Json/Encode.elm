module RichText.Json.Encode
  ( span, spanType
  , block, blockType
  ) where

import Json.Encode (..)
import RichText (..)
import List

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

blockType : BlockType -> Value
blockType t = case t of
  Heading -> string "Heading"
  Paragraph -> string "Paragraph"
  Quote -> string "Quote"
  Task -> string "Task"

block : Block -> Value
block (t,v) = object
  [ ("type", blockType t)
  , ("value", List.map span v |> list)
  ]
