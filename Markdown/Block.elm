module Markdown.Block where

import Markdown.Span as Span
import Html (Html, node, text)
import Html.Attributes (class)

data Block =
  Heading Int Span.Span | -- level, content
  Paragraph Span.Span | -- content
  CodeBlock (Maybe String) String -- language, content

type Cursor = Span.Cursor

update : (Block, Cursor) -> String -> Block
update (value, cursor) char = case value of
  Heading h span -> Heading h <| Span.update (span, cursor) char
  Paragraph span -> Paragraph <| Span.update (span, cursor) char
  CodeBlock lang s -> CodeBlock lang <| Span.updateString (s, cursor) char

move (value, cursor) char = case value of
  Heading _ span -> Span.move (span, cursor) char
  Paragraph span -> Span.move (span, cursor) char
  CodeBlock _ s -> Span.moveString (s, cursor) char

render : Block -> Maybe Cursor -> Html
render block mc = case block of
  Heading 1 span -> node "h1" [] [ Span.render span mc ]
  Heading _ span -> node "h1" [] [ Span.render span mc ]
  Paragraph span -> node "p" [] [ Span.render span mc ]
  CodeBlock _ s -> node "code" [] [ text s ] -- TODO render cursor
