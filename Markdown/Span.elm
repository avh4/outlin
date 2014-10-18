module Markdown.Span where

import String
import Html (Html, node, text)
import Html.Attributes (class)

type Foo a b = {
  update: (a, b) -> String -> a,
  move: (a, b) -> String -> b
  }


data Span =
  Plain String

type StringCursor = Int
type Cursor = Int

stringer : Foo String StringCursor
stringer =
  { update = \(value, selection) char ->
    (String.left selection value)
    ++ char
    ++ (String.dropLeft selection value)
  , move = \(value, selection) char ->
    selection + 1 -- TODO lenght of char
  }

spanner : Foo Span Cursor
spanner =
  { update = \(value, selection) char -> case value of
    Plain s -> Plain <| stringer.update (s, selection) char
  , move = \(value, selection) char -> case value of
    Plain s -> stringer.move (s, selection) char
  }

update = spanner.update

move = spanner.move

render : Span -> Maybe Cursor -> Html
render span mc = case span of
  Plain string -> case mc of
    Just cursor -> node "span" [] [
      text <| String.left cursor string,
      node "span" [ class "cursor" ] [ text "^" ],
      text <| String.dropLeft cursor string ]
    Nothing -> node "span" [] [ text string ]

