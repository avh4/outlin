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

changeAt : (a -> a) -> Int -> [a] -> [a]
changeAt fn index list =
  indexedMap (\i item -> if i == index then fn item else item) list

update : Entry -> Cursor -> String -> Entry
update value cursor char = case value of Entry e -> case cursor of
  InText i -> Entry { e | text <- Core.String.update e.text i char }
  InDescription i -> Entry { e | description <- Core.String.update e.description i char }
  InChild i c -> Entry { e | children <- changeAt (\x -> update x c char) i e.children }

at i list = list |> drop i |> head

move : Entry -> Cursor -> String -> Cursor
move value cursor char = case value of Entry e -> case cursor of
  InText i -> InText <| Core.String.move e.text i char
  InDescription i -> InDescription <| Core.String.move e.description i char
  InChild i child -> InChild i <| move (at i e.children ) child char

goLeft : Cursor -> Cursor
goLeft cursor = case cursor of
  InText n -> InText (n-1)
  _ -> cursor

goRight : Cursor -> Cursor
goRight cursor = case cursor of
  InText n -> InText (n+1)
  _ -> cursor

goNext value cursor = case cursor of
  InText i -> InDescription i
  InDescription i -> InChild 0 (InText i)
  InChild n (InText i) -> InChild n (InDescription i)
  InChild n (InDescription i) -> InChild (n+1) (InText i)
  InChild n c -> InChild (n+1) c

goPrev value cursor = case cursor of
  InChild 0 c -> c
  InChild i c -> InChild (i-1) c
  InDescription i -> InText i
  InText _ -> InText 0

toTextCursor : Maybe Cursor -> Maybe Core.String.Cursor
toTextCursor mc = case mc of
  Just (InText i) -> Just i
  _ -> Nothing

toDescriptionCursor : Maybe Cursor -> Maybe Core.String.Cursor
toDescriptionCursor mc = case mc of
  Just (InDescription i) -> Just i
  _ -> Nothing


render : Entry -> Maybe Cursor -> Html
render value mc = case value of
  Entry e -> node "li" [] [
    Core.String.render e.text (toTextCursor mc),
    node "i" [] [ Core.String.render e.description (toDescriptionCursor mc)],
    node "ul" [] <| map (\x -> render x Nothing) e.children
    ]