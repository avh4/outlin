module RichText
  ( Span, SpanType(..)
  , span, bold, link
  , toMarkdown
  , Block, BlockType(..)
  , paragraph, heading
  , toPlainText
  ) where

import String
import List

type SpanType
  = Normal
  | Bold
  | Link String

type alias Span = (SpanType, String)

span : String -> Span
span s = (Normal, s)

bold : String -> Span
bold s = (Bold, s)

link : String -> String -> Span
link url s = (Link url, s)

toMarkdown : Span -> String
toMarkdown (spanType,s) = case spanType of
  Normal -> s
  Bold -> "**" ++ s ++ "**"
  Link url -> "[" ++ s ++ "](" ++ url ++ ")"


type BlockType
  = Heading
  | Paragraph
  | Quote
  | Task

type alias Block = (BlockType, List Span)

paragraph : String -> Block
paragraph s = (Paragraph, [span s])

heading : String -> Block
heading s = (Heading, [span s])

toPlainText : Block -> String
toPlainText (t,v) = String.join "" (List.map toMarkdown v)
