module Markdown.Block where

import Markdown.Span as Span
import Html (Html, node, text)
import Html.Attributes (class)

data Block =
  Heading Int Span.Span | -- level, content
  Paragraph Span.Span | -- content
  CodeBlock (Maybe String) String -- language, content

type Cursor = Span.Cursor

update (value, cursor) char = case value of
  Paragraph span -> Paragraph <| Span.update (span, cursor) char

move (value, cursor) char = case value of
  Paragraph span -> Span.move (span, cursor) char

render : Block -> Maybe Cursor -> Html
render block mc = case block of
  Paragraph span -> node "p" [] [ Span.render span mc ]

