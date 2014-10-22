module Outline.Entry where

import Html (Html, node, text)
import Html.Attributes (class)
import Core.String

data Entry = Entry {
  text:String,
  description:String,
  children:[Entry]
  }

data Cursor =
  InText Int |
  InDescription Int |
  InChild Int Cursor

update : Entry -> Cursor -> String -> Entry
update value cursor char = case value of Entry e -> case cursor of
  InText i -> Entry { e | text <- Core.String.update e.text i char }
  InDescription i -> Entry { e | description <- Core.String.update e.description i char }
--  InChild i c -> Entry { e | children <- changeAt ... }
  InChild _ _ -> value -- TODO

move : Entry -> Cursor -> String -> Cursor
move value cursor char = cursor -- TODO

render : Entry -> Maybe Cursor -> Html
render value mc = case value of
  Entry e -> node "li" [] [
    text e.text,
    node "i" [] [ text e.description],
    node "ul" [] <| map (\x -> render x Nothing) e.children
    ]