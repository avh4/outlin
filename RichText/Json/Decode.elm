module RichText.Json.Decode
  ( span, spanType
  , block, blockType
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

blockTypeFromString : String -> Result String BlockType
blockTypeFromString s = case s of
  "Heading" -> Ok Heading
  "Paragraph" -> Ok Paragraph
  "Quote" -> Ok Quote
  "Task" -> Ok Task
  _ -> Err ("Invalid block type: " ++ s)

blockType : Decoder BlockType
blockType = customDecoder string blockTypeFromString

block : Decoder Block
block = object2 (,)
  ("type" := blockType)
  ("value" := list span)
