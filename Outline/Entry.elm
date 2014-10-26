module Outline.Entry (Base(Entry), Entry, BaseCursor(..), Cursor, entry, insert, backspace, enter, addInboxItem, delete, goLeft, goRight, goNext, goPrev, render, decoder, toJson) where

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

findLastCursor en = case en of
  Entry e -> if
    | length e.children > 0 -> InChild (-1+length e.children,InText 0)
    | length e.inbox > 0 -> InInbox (-1+length e.inbox,0)
    | otherwise -> InText 0

addInboxItem : EntryAction
addInboxItem en cur = case en of Entry e -> case cur of
  InChild c -> case Core.Array.do (InText 0) (\_ -> InText 0) addInboxItem e.children c of
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
    Action.EnterPrev -> Action.EnterPrev
    Action.EnterNext -> if
      | length e.inbox > 0 -> Action.Update en <| InInbox (0,0)
      | length e.children > 0 -> Action.Update en <| InChild (0, (InText c))
      | otherwise -> Action.EnterNext
  InDescription c -> case stringAction e.description c of
    Action.Update newV newCur -> Action.Update (Entry { e | description <- newV }) (InDescription newCur)
    Action.Delete -> Action.Delete
    Action.NoChange -> Action.NoChange
  InInbox c -> case Core.Array.do 0 (\_ -> 0) stringAction e.inbox c of
    Action.Update newList newCur -> Action.Update (Entry { e | inbox <- newList }) (InInbox newCur)
    Action.Delete -> Action.Update (Entry { e | inbox <- [] }) (InText <| String.length e.text)
    Action.NoChange -> Action.NoChange
    Action.EnterNext -> if
      | length e.children > 0 -> Action.Update en <| InChild (0,InText 0)
      | otherwise -> Action.EnterNext
    Action.EnterPrev -> Action.Update en <| InText 0
  InChild c -> case Core.Array.do (InText 0) findLastCursor (do stringAction) e.children c of
    Action.Update newChildren newChildCur -> Action.Update (Entry {e | children <- newChildren}) (InChild newChildCur)
    Action.Delete -> Action.Update (Entry { e | children <- [] }) (InText <| String.length e.text)
    Action.EnterNext -> Action.EnterNext
    Action.EnterPrev -> Action.Update en <| if
      | length e.inbox > 0 -> InInbox (-1+length e.inbox,0)
      | otherwise -> InText 0
    Action.NoChange -> Action.NoChange

goLeft = do Core.String.goLeft
goRight = do Core.String.goRight
backspace = do Core.String.backspace
delete = do Core.String.delete

insert : String -> EntryAction
insert s = do (Core.String.insert s)

goNext = do (Action.always Action.EnterNext)
goPrev = do (Action.always Action.EnterPrev)

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
