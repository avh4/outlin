module Main where

import Html (Html, text, node, toElement)
import Html.Attributes (class)

import String
import Keys
import Char
import Debug
import Outline.Entry as Entry


type Document = Entry.Entry

---- App

---- Test input

type DocumentCursor = Entry.Cursor
type Model = { value:Document, selection:DocumentCursor }

goLeft : DocumentCursor -> DocumentCursor
goLeft cursor = case cursor of
  Entry.InText n -> Entry.InText (n-1)
  _ -> cursor

goRight : DocumentCursor -> DocumentCursor
goRight cursor = case cursor of
  Entry.InText n -> Entry.InText (n+1)
  _ -> cursor

liftArrayTuple : ([a], b) -> [(a,b)]
liftArrayTuple (aa, b) =
  map (\a -> (a,b)) aa

updateDocument : Document -> DocumentCursor -> String -> Document
updateDocument = Entry.update 
--    changeAt (\(s,c) -> Block.update (s,snd c) char) (\(s,c) -> s) (fst cursor) (liftArrayTuple (value, cursor))

moveDocument : Document -> DocumentCursor -> String -> DocumentCursor
moveDocument = Entry.move
--    (fst cursor, Block.move (head value, snd cursor) char)

changeAt : (a -> b) -> (a -> b) -> Int -> [a] -> [b]
changeAt fn1 fn2 index list =
  indexedMap (\i item -> if i == index then fn1 item else fn2 item) list

insertInModel : Model -> String -> Model
insertInModel {value,selection} char =
  let a = updateDocument value selection char
      b = moveDocument  value selection char
  in {value=a, selection=b}

-- INPUT

apk : Keys.KeyInput -> Model -> Model
apk key last = case key of
  Keys.Left -> { last | selection <- goLeft last.selection }
  Keys.Right -> { last | selection <- goRight last.selection }
  Keys.Enter -> last
  Keys.Character s -> insertInModel last s
  Keys.Nothing -> last

-- RENDER

renderDocument : Document -> DocumentCursor -> Html
renderDocument value cursor = Entry.render value (Just cursor)

--  changeAt (\s -> Block.render s <| Just <| snd cursor) (\s -> Block.render s Nothing) (fst cursor) blocks
--  |> node "div" []

renderModel : Model -> Html
renderModel m = renderDocument m.value m.selection


aa = foldp apk (Model
  (Entry.Entry { text="Text", description="Desc", children=[(Entry.Entry { text="Text", description="Desc", children=[] })] })
  (Entry.InText 4)) Keys.lastPressed

main = (toElement 800 600) <~ (renderModel <~ aa)