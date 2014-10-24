module Main where

import Html (Html, text, node, toElement)
import Html.Attributes (class)

import String
import Keys
import Char
import Debug
import Outline.Entry as Entry
import SampleData
import Json.Decoder
import Json.Process
import Json.Output


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

updateModel : (c -> val -> cur -> val) -> (c -> val -> cur -> cur) -> c -> {value:val, selection:cur} -> {value:val, selection:cur}
updateModel valueFn curFn char {value,selection} =
  let a = valueFn char value selection
      b = curFn char value selection
  in {value=a, selection=b}

insertInModel : Model -> String -> Model
insertInModel m s = updateModel updateDocument moveDocument s m

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
renderModel m = node "div" [] [ renderDocument m.value m.selection ]

port pressesIn : Signal String
port downsIn : Signal Int

data Command =
  KeyPress String |
  KeyDown Int |
  Loaded String

step : Command -> Model -> Model
step c m = case c of
  KeyPress char -> apk (Keys.fromPresses char) m
  KeyDown code -> apk (Keys.fromDowns code) m
  Loaded s -> case Json.Decoder.fromString s `Json.Process.into` Entry.decoder of
    Json.Output.Success doc -> { value=doc, selection=Entry.InText 0 }
    _ -> m

commands : Signal Command
commands = merges [
  KeyPress <~ pressesIn,
  KeyDown <~ downsIn,
  Loaded <~ dropboxIn
  ]

aa = foldp step (Model SampleData.template (Entry.InText 4)) commands

main = (toElement 800 600) <~ (renderModel <~ aa)

port dropboxOut : Signal String
port dropboxOut = dropRepeats <| (\x -> Entry.toJson x.value) <~ aa

port dropboxIn : Signal String
