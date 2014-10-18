module Markdown.Span where

import String
import Html (Html, node, text)
import Html.Attributes (class)

data Span =
  Plain String

type StringCursor = Int
type Cursor = Int

stringer =
  { update = \(value, selection) char ->
    (String.left selection value)
    ++ char
    ++ (String.dropLeft selection value)
  , move = \(value, selection) char ->
    selection + 1 -- TODO lenght of char
  }

updateString = stringer.update

moveString = stringer.move

update : (Span, Cursor) -> String -> Span
update (value, selection) char = case value of
  Plain s -> Plain <| stringer.update (s, selection) char

move : (Span, Cursor) -> String -> Cursor
move (value, selection) char = case value of
  Plain s -> stringer.move (s, selection) char

render : Span -> Maybe Cursor -> Html
render span mc = case span of
  Plain string -> case mc of
    Just cursor -> node "span" [] [
      text <| String.left cursor string,
      node "span" [ class "cursor" ] [ text "^" ],
      text <| String.dropLeft cursor string ]
    Nothing -> node "span" [] [ text string ]

