module Outline.Entry (Base(Entry), Entry, BaseCursor(..), Cursor, entry, insert, backspace, enter, addInboxItem, delete, goLeft, goRight, goNextAction, goPrevAction, render, decoder, toJson) where

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
import Debug

data Base a = Entry {
  text:a,
  description:a,
  inbox:[a],
  children:[Base a]
  }

type Entry = Base String

entry : String -> String -> [String] -> [Entry] -> Entry
entry t d i c = Entry {text=t, description=d, inbox=i, children=c}

data BaseCursor c =
  InText c |
  InDescription c |
  InInbox (Core.Array.Cursor c) |
  InChild (Core.Array.Cursor (BaseCursor c))

type Cursor = BaseCursor Core.String.Cursor

type StringAction = Action String Core.String.Cursor
type EntryAction = Action Entry Cursor

enter = do Core.String.split

addInboxItem : EntryAction
addInboxItem en cur = case en of Entry e -> case cur of
  InChild c -> case Core.Array.do addInboxItem e.children c of
    Action.Update newChildren newChildCur -> Action.Update (Entry {e | children <- newChildren}) (InChild newChildCur)
    Action.NoChange -> Action.NoChange
  _ -> Action.Update (Entry { e | inbox <- [""] ++ e.inbox }) (InInbox (0,0))

do : StringAction -> EntryAction
do stringAction en cur = case en of Entry e -> case cur of
  InText c -> case stringAction e.text c of
    Action.Update newV newCur -> Action.Update (Entry { e | text <- newV }) (InText newCur)
    Action.Delete -> Action.Delete
    Action.NoChange -> Action.NoChange
    Action.Split (left :: right :: []) newI c -> Action.Split [entry left "" [] [], Entry {e | text <- right}] newI (InText c)
    Action.Split (_ :: _ :: _) _ _ -> Debug.crash "Not yet implemented for splits > 2"
    Action.Split _ _ _ -> Debug.crash "Split has less than two children"
  InDescription c -> case stringAction e.description c of
    Action.Update newV newCur -> Action.Update (Entry { e | description <- newV }) (InDescription newCur)
    Action.Delete -> Action.Delete
    Action.NoChange -> Action.NoChange
  InInbox c -> case Core.Array.do stringAction e.inbox c of
    Action.Update newList newCur -> Action.Update (Entry { e | inbox <- newList }) (InInbox newCur)
    Action.Delete -> Action.Update (Entry { e | inbox <- [] }) (InText <| String.length e.text)
    Action.NoChange -> Action.NoChange
  InChild c -> case Core.Array.do (do stringAction) e.children c of
    Action.Update newChildren newChildCur -> Action.Update (Entry {e | children <- newChildren}) (InChild newChildCur)
    Action.Delete -> Action.Update (Entry { e | children <- [] }) (InText <| String.length e.text)
    Action.NoChange -> Action.NoChange

goLeft = do Core.String.goLeft
goRight = do Core.String.goRight
backspace = do Core.String.backspace
delete = do Core.String.delete

insert : String -> EntryAction
insert s = do (Core.String.insert s)

-- TODO: replace with Action.Result
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

go : (Entry -> Cursor -> MoveCmd) -> EntryAction
go fn value cursor = case fn value cursor of
  EnterPrev -> Action.Update value (InText 0)
  StayHere c -> Action.Update value c
  EnterNext -> Action.Update value cursor

goNextAction = go goNext

goPrev : Entry -> Cursor -> MoveCmd
goPrev e cursor = case e of Entry value -> case cursor of
  InText i -> EnterPrev
  InDescription i -> StayHere <| InText i
  InInbox (n,c) -> StayHere <| if (n > 0) then InInbox (n-1, c) else InText c
  InChild (n,c) -> goChild n goPrev value c

goPrevAction = go goPrev

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
  ++ ",\"inbox\":" ++ Core.Array.toJson Core.String.toJson e.inbox
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
