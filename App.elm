module App where

import Html (Html, text, node, toElement)
import Html.Attributes (class)

import String
import Keys
import Char
import Debug
import Outline.Entry as Entry
import Json.Decoder
import Json.Process
import Json.Output
import Core.Action (Action)
import Core.Action as Action

---- App

type Document = Entry.Entry
type DocumentCursor = Entry.Cursor
type Model = { value:Document, selection:DocumentCursor }

updateModel : Action val cur -> {value:val, selection:cur} -> {value:val, selection:cur}
updateModel action {value,selection} = case action value selection of
  Action.Update a b -> {value=a, selection=b}
  -- explicity list the following action results, which are all no-ops on document
  Action.Split _ _ _ -> {value=value, selection=selection}
  Action.Delete -> {value=value, selection=selection}
  Action.EnterNext -> {value=value, selection=selection}
  Action.EnterPrev -> {value=value, selection=selection}
  Action.NoChange -> {value=value, selection=selection}

---- INPUT

data Command
  = Key Keys.KeyInput
  | KeyMeta Int
  | Loaded String

step : Command -> Model -> Model
step c m = case c of
  Key (Keys.Left) -> updateModel Entry.goLeft m
  Key (Keys.Right) -> updateModel Entry.goRight m
  Key (Keys.Down) -> updateModel Entry.goNext m
  Key (Keys.Up) -> updateModel Entry.goPrev m
  Key (Keys.Enter) -> updateModel Entry.enter m
  Key (Keys.Character s) -> updateModel (Entry.insert s) m
  Key (Keys.Backspace) -> updateModel Entry.backspace m
  Key (Keys.Command "a") -> updateModel Entry.addInboxItem m
  Key (Keys.Command "d") -> updateModel Entry.delete m
  Loaded s -> case Json.Decoder.fromString s `Json.Process.into` Entry.decoder of
    Json.Output.Success doc -> { value=doc, selection=Entry.InText 0 }
    x -> fst (m, Debug.log "Load failed" x)
  x -> fst (m, Debug.log "Unhandled command" x)

---- RENDER

renderDocument : Document -> DocumentCursor -> Html
renderDocument value cursor = Entry.render value (Just <| Debug.watch "cursor" cursor)

renderDocs = node "div" []
  [ node "p" [] [ text "Cmd-A: add to inbox" ]
  , node "p" [] [ text "Cmd-D: delete" ]
  ]

render : Model -> Html
render m = node "div" [] [ renderDocs, renderDocument m.value m.selection ]
