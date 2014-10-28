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
  Key (Keys.Command "p") -> updateModel Entry.promote m
  Key (Keys.Command "m") -> updateModel Entry.missort m
  Key (Keys.Command "1") -> updateModel (Entry.moveInto 0) m
  Key (Keys.Command "2") -> updateModel (Entry.moveInto 1) m
  Key (Keys.Command "3") -> updateModel (Entry.moveInto 2) m
  Key (Keys.Command "4") -> updateModel (Entry.moveInto 3) m
  Key (Keys.Command "5") -> updateModel (Entry.moveInto 4) m
  Key (Keys.Command "6") -> updateModel (Entry.moveInto 5) m
  Key (Keys.Command "7") -> updateModel (Entry.moveInto 6) m
  Loaded s -> case Json.Decoder.fromString s `Json.Process.into` Entry.decoder of
    Json.Output.Success doc -> { value=doc, selection=Entry.InText 0 }
    x -> fst (m, Debug.log "Load failed" x)
  x -> fst (m, Debug.log "Unhandled command" x)

---- RENDER

renderDocument : Document -> DocumentCursor -> Html
renderDocument value cursor = Entry.render value (Just <| Debug.watch "cursor" cursor)

renderDocs = node "div" []
  [ node "p" [] [ text "⌘A: add to inbox" ]
  , node "p" [] [ text "⌘D: delete" ]
  , node "p" [] [ text "⌘P: promote from inbox" ]
  , node "p" [] [ text "⌘1 - ⌘7: move into …" ]
  , node "p" [] [ text "⌘M: Missorted" ]
  ]

render : Model -> Html
render m = node "div" [] [ renderDocs, renderDocument m.value m.selection ]
