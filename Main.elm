module Main where

import Html (Html, text, node, toElement)
import Html.Attributes (class)

import String
import Keys
import Char
import Debug
import Outline.Entry as Entry
import SampleData
import SampleJson


type Document = Entry.Entry

---- App

---- Test input

type DocumentCursor = Entry.Cursor
type Model = { value:Document, selection:DocumentCursor }

liftArrayTuple : ([a], b) -> [(a,b)]
liftArrayTuple (aa, b) =
  map (\a -> (a,b)) aa

updateDocument : String -> Document -> DocumentCursor -> Document
updateDocument = Entry.update

moveDocument : String -> Document -> DocumentCursor -> DocumentCursor
moveDocument = Entry.move

changeAt : (a -> b) -> (a -> b) -> Int -> [a] -> [b]
changeAt fn1 fn2 index list =
  indexedMap (\i item -> if i == index then fn1 item else fn2 item) list

insertInModel : Model -> String -> Model
insertInModel {value,selection} char =
  let a = updateDocument char value selection
      b = moveDocument char value selection
  in {value=a, selection=b}

-- INPUT

apk : Keys.KeyInput -> Model -> Model
apk key last = case key of
  Keys.Left -> { last | selection <- Entry.goLeft last.value last.selection }
  Keys.Right -> { last | selection <- Entry.goRight last.value last.selection }
  Keys.Down -> { last | selection <- Entry.goNext last.value last.selection }
  Keys.Up -> { last | selection <- Entry.goPrev last.value last.selection }
  Keys.Enter -> last
  Keys.Character s -> insertInModel last s
  Keys.Nothing -> last

-- RENDER

renderDocument : Document -> DocumentCursor -> Html
renderDocument value cursor = Entry.render value (Just <| Debug.watch "cursor" cursor)

--  changeAt (\s -> Block.render s <| Just <| snd cursor) (\s -> Block.render s Nothing) (fst cursor) blocks
--  |> node "div" []

renderModel : Model -> Html
renderModel m = node "div" [] [
  renderDocument m.value m.selection,
  node "code" [] [ text <| Entry.toJson m.value ],
  node "hr" [] [],
  node "code" [] [ text <| show <| Entry.fromJson SampleJson.string ]
  ]

port pressesIn : Signal String
port downsIn : Signal Int

lastPressed = merge (lift Keys.fromPresses pressesIn) (lift Keys.fromDowns downsIn)

aa = foldp apk (Model SampleData.template (Entry.InText 4)) lastPressed

main = (toElement 800 600) <~ (renderModel <~ aa)

port dropboxOut : Signal String
port dropboxOut = dropRepeats <| (\x -> Entry.toJson x.value) <~ aa
