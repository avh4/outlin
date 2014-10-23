module Outline.Entry where

import Html (Html, node, text)
import Html.Attributes (class)
import Core.String
import Core.Array
import String

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

at : Int -> [a] -> a
at i list = list |> drop i |> head

move : Entry -> Cursor -> String -> Cursor
move value cursor char = case value of Entry e -> case cursor of
  InText i -> InText <| Core.String.move e.text i char
  InDescription i -> InDescription <| Core.String.move e.description i char
  InChild i child -> InChild i <| move (at i e.children ) child char

goLeft : Cursor -> Cursor
goLeft cursor = case cursor of
  InText n -> InText <| Core.String.goLeft n
  InDescription n -> InDescription <| Core.String.goLeft n
  InChild n c -> InChild n <| goLeft c

goRight : Cursor -> Cursor
goRight cursor = case cursor of
  InText n -> InText <| Core.String.goRight n
  InDescription n -> InDescription <| Core.String.goRight n
  InChild n c -> InChild n <| goRight c

data MoveCmd = EnterPrev | StayHere Cursor | EnterNext

ll : Entry -> Int
ll e = case e of Entry value -> length value.children

goChild n fn value cursor = case fn (at n value.children) cursor of
  StayHere c -> StayHere <| InChild n c
  EnterNext -> if length value.children > n+1 then StayHere <| InChild (n+1) (InText 0)
    else EnterNext
  EnterPrev -> if | n == 0 -> StayHere <| InText 0
                  | (ll (at (n-1) value.children)) > 0 -> StayHere <| InChild (n-1) (InChild (ll (at (n-1) value.children) - 1) (InText 0))
                  | otherwise -> StayHere <| InChild (n-1) (InText 0)

gn : Entry -> Cursor -> MoveCmd
gn e cursor = case e of Entry value -> case cursor of
  InText i -> if length value.children > 0 then StayHere <| InChild 0 (InText i)
    else EnterNext
  InDescription i -> gn e (InText i)
  InChild n c -> goChild n gn value c

go : (Entry -> Cursor -> MoveCmd) -> Entry -> Cursor -> Cursor
go fn value cursor = case fn value cursor of
  EnterPrev -> InText 0
  StayHere c -> c
  EnterNext -> cursor

goNext = go gn

gp : Entry -> Cursor -> MoveCmd
gp e cursor = case e of Entry value -> case cursor of
  InText i -> EnterPrev
  InDescription i -> StayHere <| InText i
  InChild n c -> goChild n gp value c

goPrev = go gp

toTextCursor : Maybe Cursor -> Maybe Core.String.Cursor
toTextCursor mc = case mc of
  Just (InText i) -> Just i
  _ -> Nothing

toDescriptionCursor : Maybe Cursor -> Maybe Core.String.Cursor
toDescriptionCursor mc = case mc of
  Just (InDescription i) -> Just i
  _ -> Nothing

toChildrenCursor : Maybe Cursor -> Maybe (Core.Array.Cursor Cursor)
toChildrenCursor mc = case mc of
  Just (InChild n c) -> Just (n, c)
  _ -> Nothing

render : Entry -> Maybe Cursor -> Html
render value mc = case value of
  Entry e -> node "li" [] [
    Core.String.render e.text (toTextCursor mc),
    node "i" [] [ Core.String.render e.description (toDescriptionCursor mc)],
    node "ul" [] <| Core.Array.render render e.children (toChildrenCursor mc)
    ]