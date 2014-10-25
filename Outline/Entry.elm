module Outline.Entry where

import Html (Html, node, text)
import Html.Attributes (class)
import Core.String
import Core.Array
import String
import Json.Decoder
import Json.Decoder (..)
import Json.Output
import Core.Action (Action)

data Entry = Entry {
  text:String,
  description:String,
  children:[Entry]
  }

data Cursor =
  InText Core.String.Cursor |
  InDescription Core.String.Cursor |
  InChild (Core.Array.Cursor Cursor)

update : String -> Entry -> Cursor -> Entry
update char value cursor = case value of Entry e -> case cursor of
  InText i -> Entry { e | text <- (Core.String.insertAction char).valueFn e.text i }
  InDescription i -> Entry { e | description <- (Core.String.insertAction char).valueFn e.description i }
  InChild c -> Entry { e | children <- (Core.Array.applyAt <| Action (update char) (\_ cc -> cc)).valueFn e.children c }

at : Int -> [a] -> a
at i list = list |> drop i |> head

ddd : (String -> Core.String.Cursor -> Core.String.Cursor) -> Entry -> Cursor -> Cursor
ddd fn entry cursor = case entry of Entry e -> case cursor of
  InText n -> InText <| fn e.text n
  InDescription n -> InDescription <| fn e.description n
  InChild c -> InChild <| (Core.Array.applyAt <| Action (\v _ -> v) (ddd fn)).curFn e.children c

move : String -> Entry -> Cursor -> Cursor
move char = ddd (Core.String.insertAction char).curFn

insertAction : String -> Action Entry Cursor
insertAction s = Action (update s) (move s)

goLeftAction : Action Entry Cursor
goLeftAction = Action (\v _ -> v) (ddd Core.String.goLeft)

goRightAction = Action (\v _ -> v) (ddd Core.String.goRight)

data MoveCmd = EnterPrev | StayHere Cursor | EnterNext

ll : Entry -> Int
ll e = case e of Entry value -> length value.children

goChild n fn value cursor = case fn (at n value.children) cursor of
  StayHere c -> StayHere <| InChild (n,c)
  EnterNext -> if length value.children > n+1 then StayHere <| InChild (n+1, (InText 0))
    else EnterNext
  EnterPrev -> if | n == 0 -> StayHere <| InText 0
                  | (ll (at (n-1) value.children)) > 0 -> StayHere <| InChild (n-1, (InChild (ll (at (n-1) value.children) - 1, (InText 0))))
                  | otherwise -> StayHere <| InChild (n-1, (InText 0))

goNext : Entry -> Cursor -> MoveCmd
goNext e cursor = case e of Entry value -> case cursor of
  InText i -> if length value.children > 0 then StayHere <| InChild (0, (InText i))
    else EnterNext
  InDescription i -> goNext e (InText i)
  InChild (n, c) -> goChild n goNext value c

go : (Entry -> Cursor -> MoveCmd) -> Entry -> Cursor -> Cursor
go fn value cursor = case fn value cursor of
  EnterPrev -> InText 0
  StayHere c -> c
  EnterNext -> cursor

goNextAction : Action Entry Cursor
goNextAction = Action (\v _ -> v) (go goNext)

goPrev : Entry -> Cursor -> MoveCmd
goPrev e cursor = case e of Entry value -> case cursor of
  InText i -> EnterPrev
  InDescription i -> StayHere <| InText i
  InChild (n,c) -> goChild n goPrev value c

goPrevAction = Action (\v _ -> v) (go goPrev)

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
  Just (InChild (n,c)) -> Just (n, c)
  _ -> Nothing

render : Entry -> Maybe Cursor -> Html
render value mc = case value of
  Entry e -> node "li" [] [
    Core.String.render e.text (toTextCursor mc),
    node "i" [] [ Core.String.render e.description (toDescriptionCursor mc)],
    node "ul" [] <| Core.Array.render render e.children (toChildrenCursor mc)
    ]


---- JSON

toJson : Entry -> String
toJson entry = case entry of Entry e ->
  "{\"text\":" ++ Core.String.toJson e.text
  ++ ",\"description\":" ++ Core.String.toJson e.description
  ++ ",\"children\":" ++ Core.Array.toJson toJson e.children
  ++ "}"

decoder : Json.Decoder.Decoder Entry
decoder a = Json.Decoder.decode3
  ("text" := Json.Decoder.string)
  ("description" := Json.Decoder.string)
  ("children" := Json.Decoder.listOf decoder)
  (\t d c -> Entry {text=t,description=d,children=c})
  a -- this is to work around https://github.com/elm-lang/Elm/issues/639
