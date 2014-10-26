module Outline.Entry (Base(Entry), Entry, BaseCursor(..), Cursor, entry, insertAction, backspace, enter, addInboxItem, goLeftAction, goRightAction, goNextAction, goPrevAction, render, decoder, toJson) where

import Html (Html, node, text)
import Html.Attributes (class)
import Core.String
import Core.Array
import String
import Json.Decoder
import Json.Decoder (..)
import Json.Output
import Core.Action (Action)
import Core.Action as Action

data Base a = Entry {
  text:a,
  description:a,
  inbox:[a],
  children:[Base a]
  }

type Entry = Base String

entry : String -> String -> [Entry] -> Entry
entry t d c = Entry {text=t, description=d, inbox=[], children=c}

data BaseCursor c =
  InText c |
  InDescription c |
  InInbox (Core.Array.Cursor c) |
  InChild (Core.Array.Cursor (BaseCursor c))

type Cursor = BaseCursor Core.String.Cursor

type StringAction = Action String Core.String.Cursor
type EntryAction = Action Entry Cursor

--doText : (String -> x) -> ()

uupdate : StringAction -> Entry -> Cursor -> Entry
uupdate action value cursor = case value of Entry e -> case cursor of
  InText i -> Entry { e | text <- action.valueFn e.text i }
  InDescription i -> Entry { e | description <- action.valueFn e.description i }
  InInbox c -> Entry { e | inbox <- (Core.Array.applyAt action).valueFn e.inbox c }
  InChild c -> Entry { e | children <- (Core.Array.applyAt <| Action (uupdate action) (\_ cc -> cc)).valueFn e.children c }

-- navTo : (String -> x) -> (String -> x) -> (Core.Array.Cursor x -> x) -> Entry -> Cursor -> x
-- navTo textFn descFn childFn en cur = case en of Entry e -> case cur of
--   InText n -> textFn e.text
--   InDescription n -> descFn e.description
--   InChild c -> childFn --(Core.Array.applyAt <| Action (\v _ -> v) (mmove action)).curFn e.children c

-- straightNav : (String -> Core.String.Cursor) -> Entry -> Cursor -> Cursor
-- straightNav fn = navTo
--   (\s -> InText <| fn s)
--   (\s -> InDescription <| fn s)
--   (\(i,c) -> InChild (i,c))

-- mmove : StringAction -> Entry -> Cursor -> Cursor
-- mmove action = straightNav (\n -> mmmove action n)

mmove : StringAction -> Entry -> Cursor -> Cursor
mmove action entry cursor = case entry of Entry e -> case cursor of
  InText n -> InText <| action.curFn e.text n
  InDescription n -> InDescription <| action.curFn e.description n
  InInbox c -> InInbox <| (Core.Array.applyAt action).curFn e.inbox c
  InChild c -> InChild <| (Core.Array.applyAt <| Action (\v _ -> v) (mmove action)).curFn e.children c

liftAction : StringAction -> EntryAction
liftAction sa = Action (uupdate sa) (mmove sa)

liftCursorAction : StringAction -> EntryAction
liftCursorAction sa = Action (\v _ -> v) (mmove sa)

insertAction : String -> EntryAction
insertAction s = liftAction (Core.String.insertAction s)

backspace : EntryAction
backspace = liftAction Core.String.backspace

ss_split : String -> Core.String.Cursor -> (String, String, Core.String.Cursor)
ss_split = (\s n -> (String.left n s, String.dropLeft n s, 0))

s_split : Entry -> Cursor -> ([Entry], Core.Array.Cursor Cursor)
s_split en cur = case en of Entry e -> case cur of
  InText n -> case ss_split e.text n of
    (left, right, c) -> ([entry left "" [], Entry {e | text <- right}], (1, InText c))
  InChild (n,c) -> case Action.apply (Core.Array.do s_split) e.children (n,c) of
    (newChildren, newChildCur) -> ([Entry {e | children <- newChildren}], (0, InChild newChildCur))
  _ -> ([en], (0, cur))

e_enter : Entry -> Cursor -> (Entry, Cursor)
e_enter en cur = case en of Entry e -> case cur of
  InChild c -> case Action.apply (Core.Array.do s_split) e.children c of
    (newChildren, newChildCur) -> (Entry {e | children <- newChildren}, InChild newChildCur)
  _ -> (en, cur) -- can't split root node

enter : EntryAction
enter = Action.split e_enter

addInboxItem_ : Entry -> Cursor -> (Entry, Cursor)
addInboxItem_ en cur = case en of Entry e -> case cur of
  InChild c -> case Action.apply (Core.Array.applyAt (Action.split addInboxItem_)) e.children c of
    (newChildren, newChildCur) -> (Entry {e | children <- newChildren}, InChild newChildCur)
  _ -> (Entry { e | inbox <- [""] ++ e.inbox }, InInbox (0,0))

addInboxItem : EntryAction
addInboxItem = Action.split addInboxItem_

goLeftAction = liftCursorAction Core.String.goLeft
goRightAction = liftCursorAction Core.String.goRight

data MoveCmd = EnterPrev | StayHere Cursor | EnterNext

ll : Entry -> Int
ll e = case e of Entry value -> length value.children

at : Int -> [a] -> a
at i list = list |> drop i |> head

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
  InInbox (n,c) -> StayHere <| InInbox (min ((length value.inbox)-1) (n+1), c)
  InChild (n,c) -> goChild n goNext value c

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
  InInbox (n,c) -> StayHere <| if (n > 0) then InInbox (n-1, c) else InText c
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

toInboxCursor : Maybe Cursor -> Maybe (Core.Array.Cursor Core.String.Cursor)
toInboxCursor mc = case mc of
  Just (InInbox (n,c)) -> Just <| Core.Array.cursor n c
  _ -> Nothing

toChildrenCursor : Maybe Cursor -> Maybe (Core.Array.Cursor Cursor)
toChildrenCursor mc = case mc of
  Just (InChild (n,c)) -> Just <| Core.Array.cursor n c
  _ -> Nothing

render : Entry -> Maybe Cursor -> Html
render value mc = case value of
  Entry e -> node "li" [] [
    Core.String.render e.text (toTextCursor mc),
    node "i" [] [ Core.String.render e.description (toDescriptionCursor mc)],
    node "ol" [] <| map (\x -> node "li" [] [x]) <| Core.Array.render Core.String.render e.inbox (toInboxCursor mc),
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
decoder a = Json.Decoder.decode4
  ("text" := Json.Decoder.string)
  ("description" := Json.Decoder.string)
  ("inbox" := Json.Decoder.listOf Json.Decoder.string)
  ("children" := Json.Decoder.listOf decoder)
  (\t d i c -> Entry {text=t,description=d,inbox=i,children=c})
  a -- this is to work around https://github.com/elm-lang/Elm/issues/639
