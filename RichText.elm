module RichText
  ( Span, SpanType(..)
  , span, bold, link
  , toMarkdown
  , SpanZipper
  , toValue, zipper
  ) where

import Core.String as String

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

-- Zippers
-- TODO: split to separate module

type alias SpanZipper = (SpanType, String.Zipper)

toValue : SpanZipper -> Span
toValue (t,z) = (t, String.toValue z)

zipper : (String -> String.Zipper) -> Span -> SpanZipper
zipper fn (t,s) = (t, fn s)
