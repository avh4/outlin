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
import Core.Action (Action)
import Core.Action as Action


type Document = Entry.Entry

---- App

---- Test input

type DocumentCursor = Entry.Cursor
type Model = { value:Document, selection:DocumentCursor }

liftArrayTuple : ([a], b) -> [(a,b)]
liftArrayTuple (aa, b) =
  map (\a -> (a,b)) aa

changeAt : (a -> b) -> (a -> b) -> Int -> [a] -> [b]
changeAt fn1 fn2 index list =
  indexedMap (\i item -> if i == index then fn1 item else fn2 item) list

updateModel : Action val cur -> {value:val, selection:cur} -> {value:val, selection:cur}
updateModel action {value,selection} = case action value selection of
  Action.Update a b -> {value=a, selection=b}
  Action.Split _ _ _ -> {value=value, selection=selection}
  Action.Delete -> {value=value, selection=selection}
  Action.EnterNext -> {value=value, selection=selection}
  Action.EnterPrev -> {value=value, selection=selection}
  Action.NoChange -> {value=value, selection=selection}

---- INPUT

apk : Keys.KeyInput -> Model -> Model
apk key last = case key of
  Keys.Left -> updateModel Entry.goLeft last
  Keys.Right -> updateModel Entry.goRight last
  Keys.Down -> updateModel Entry.goNext last
  Keys.Up -> updateModel Entry.goPrev last
  Keys.Enter -> updateModel Entry.enter last
  Keys.Character s -> updateModel (Entry.insert s) last
  Keys.Backspace -> updateModel Entry.backspace last
  Keys.Nothing -> last

---- RENDER

renderDocument : Document -> DocumentCursor -> Html
renderDocument value cursor = Entry.render value (Just <| Debug.watch "cursor" cursor)

renderDocs = node "div" []
  [ node "p" [] [ text "Cmd-A: add to inbox" ]
  , node "p" [] [ text "Cmd-D: delete" ]
  ]

renderModel : Model -> Html
renderModel m = node "div" [] [ renderDocs, renderDocument m.value m.selection ]

port pressesIn : Signal String
port downsIn : Signal Int
port metaIn : Signal Int

data Command =
  KeyPress String |
  KeyDown Int |
  KeyMeta Int |
  Loaded String

step : Command -> Model -> Model
step c m = case c of
  KeyPress char -> apk (Keys.fromPresses char) m
  KeyDown code -> apk (Keys.fromDowns code) m
  KeyMeta 65 -> updateModel Entry.addInboxItem m
  KeyMeta 68 -> updateModel Entry.delete m
  Loaded s -> case Json.Decoder.fromString s `Json.Process.into` Entry.decoder of
    Json.Output.Success doc -> { value=doc, selection=Entry.InText 0 }
    x -> fst (m, Debug.log "Load failed" x)
  x -> fst (m, Debug.log "Extra command" x)

commands : Signal Command
commands = merges
  [ KeyPress <~ pressesIn
  , KeyDown <~ downsIn
  , KeyMeta <~ metaIn
  , Loaded <~ dropboxIn
  ]

aa = foldp step (Model SampleData.template (Entry.InText 4)) commands

main = (toElement 800 600) <~ (renderModel <~ aa)

port dropboxOut : Signal String
port dropboxOut = dropRepeats <| (\x -> Entry.toJson x.value) <~ aa

port dropboxIn : Signal String
