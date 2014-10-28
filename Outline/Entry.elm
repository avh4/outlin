module Outline.Entry (Base(Entry), Entry, BaseCursor(..), Cursor, entry, insert, backspace, enter, addInboxItem, promote, moveInto, missort, moveChildUp, delete, goLeft, goRight, goNext, goPrev, render, decoder, toJson) where

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

addInboxItem__ : String -> Entry -> Entry
addInboxItem__ s en = case en of Entry e -> Entry { e | inbox <- [s] ++ e.inbox }

addInboxItem_ : String -> EntryAction
addInboxItem_ s en cur = case en of Entry e -> case cur of
  -- InChild _ -> Action.NoChange
  _ -> Action.Update (addInboxItem__ s en) (InInbox (0,0))

addInboxItem = doEntry (addInboxItem_ "")

dropAt : Int -> [a] -> [a]
dropAt i list = (take i list) ++ (drop (i+1) list)

changeAt : (a -> a) -> Int -> [a] -> [a]
changeAt fn i list = indexedMap (\n x -> if n == i then fn x else x) list

at : Int -> [a] -> a
at i list = head <| drop i list

promote_ : EntryAction
promote_ en cur = case en of Entry e -> case cur of
  InInbox (i,c) -> Action.Update (Entry { e
    | inbox <- dropAt i e.inbox
    , children <- (entry (at i e.inbox) "" [] []):: e.children
    }) (let newI = min i (length e.inbox-2) in if newI >= 0 then InInbox (newI,c) else InChild (0,InText c))
  _ -> Action.NoChange

promote = doEntry promote_

unwrap en = case en of Entry e -> e
getInbox en = unwrap en |> .inbox

findFirstChildInbox : Entry -> Cursor
findFirstChildInbox en = case en of
  Entry e -> e.children
    |> indexedMap (\i ee -> if length (getInbox ee) > 0 then Just i else Nothing)
    |> filterMap identity
    |> head |> \i -> InChild (i,InInbox(0,9))

moveInto_ : Int -> EntryAction
moveInto_ n en cur = case en of Entry e -> case cur of
  InInbox (i,c) -> if
    | length e.children <= n -> Action.NoChange
    | otherwise -> let newE = (Entry { e
        | inbox <- dropAt i e.inbox
        , children <- changeAt (\ee -> addInboxItem__ (at i e.inbox) ee) n e.children
        })
      in Action.Update newE (let newI = min i (length e.inbox-2) in if newI >= 0 then InInbox (newI,c) else findFirstChildInbox newE)
  _ -> Action.NoChange

moveInto n = doEntry (moveInto_ n)

removeInboxItem : EntryAction
removeInboxItem en cur = case en of Entry e -> case cur of
  InInbox (i,c) ->
    let newE = (Entry { e | inbox <- dropAt i e.inbox })
        newI = min i (length e.inbox - 2)
    in Action.Update newE (if newI >= 0 then InInbox (newI,c) else InText 0)
  _ -> Action.NoChange

updateActiveChild : EntryAction -> EntryAction
updateActiveChild action en cur = case en of Entry e -> case cur of
  InChild (n,c) ->
    let child = at n e.children
    in case action child c of
      Action.Update newChild newC ->
        Action.Update (Entry { e | children <- changeAt (\_ -> newChild) n e.children }) (InChild (n,newC))

missort_ : EntryAction
missort_ en cur = case en of Entry e -> case cur of
  InChild (n,InInbox (i,c)) ->
    let item = e.children |> at n |> getInbox |> at i
    in case updateActiveChild removeInboxItem en cur of
      Action.Update en' cur' -> case addInboxItem_ item en' cur' of
        Action.Update en'' cur'' -> Action.Update en'' cur'
        _ -> Action.NoChange
      _ -> Action.NoChange
  _ -> Action.NoChange

missort = doEntry missort_

swap : Int -> a -> Int -> a -> [a] -> [a]
swap ai a bi b list = list |> indexedMap
  (\i x -> if
    | i == ai -> b
    | i == bi -> a
    | otherwise -> x)

swapChildren : (Int -> Int) -> EntryAction
swapChildren produceToIndex en cur = case en of Entry e -> case cur of
  InChild (_,InChild _) -> Action.NoChange
  InChild (n,c) ->
    let aIndex = n
        bIndex = produceToIndex n
        a = e.children |> at aIndex
        b = e.children |> at bIndex
        newChildren = e.children |> swap aIndex a bIndex b
    in Action.Update (Entry { e | children <- newChildren }) (InChild (bIndex,c))
  _ -> Action.NoChange

moveChildUp = doEntry <| swapChildren (\n -> n-1 |> max 0)

doEntry : EntryAction -> EntryAction
doEntry action en cur = case en of Entry e -> case cur of
  InChild c -> case action en cur of
    Action.NoChange -> case Core.Array.do (InText 0) findLastCursor (doEntry action) e.children c of
      Action.Update newChildren newChildCur -> Action.Update (Entry {e | children <- newChildren}) (InChild newChildCur)
      Action.NoChange -> Action.NoChange
    x -> x
  _ -> action en cur

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

---- RENDER

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
  Entry e -> node "li" []
    [ Core.String.render e.text (toTextCursor mc)
    , node "i" [] [ Core.String.render e.description (toDescriptionCursor mc)]
    , node "ul" [] <| map (\x -> node "li" [] [x]) <| Core.Array.render Core.String.render e.inbox (toInboxCursor mc)
    , node "ol" [] <| Core.Array.render render e.children (toChildrenCursor mc)
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
