module RichText
  ( Span, SpanType(..)
  , span, bold, link
  , toMarkdown
  ) where

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
