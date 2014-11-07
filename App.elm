module App where

import Html (Html, text, node, toElement)
import Html.Attributes (class)

import String
import Keys
import Char
import Debug
import Outline.Entry as Entry
import Outline.Entry (Entry)
import Json.Decoder
import Json.Process
import Json.Output
import Core.Action (Action)
import Core.Action as Action
import Color (..)
import Text
import Outline.Document as Document

---- App

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

step : Command -> Document.Zipper -> Document.Zipper
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
  Key (Keys.Command "Up") -> updateModel Entry.moveChildUp m
  Key (Keys.Command "Down") -> updateModel Entry.moveChildDown m
  Loaded s -> case Json.Decoder.fromString s `Json.Process.into` Entry.decoder of
    Json.Output.Success doc -> { value=doc, selection=Entry.InText 0 }
    x -> fst (m, Debug.log "Load failed" x)
  x -> fst (m, Debug.log "Unhandled command" x)

---- RENDER

hintText : String -> Element
hintText s = s |> toText |> Text.italic |> Text.color (hsl 0 0 0.7) |> leftAligned

inboxItem : Entry -> Element
inboxItem e = plainText (Entry.unwrap e).text

leftPanel : (Int,Int) -> Entry -> Element
leftPanel (w,h) en =
  let e = Entry.unwrap en
  in flow down (
  [ subtitle (w,h) en
  , "⌘A: add to inbox" |> hintText
  ] ++ map inboxItem e.inbox)
  |> container w h topLeft

child : Int -> Entry -> Element
child i c = flow right
  [ "⌘" ++ (show <| i+1) ++ " " |> hintText
  , plainText (Entry.unwrap c).text
  ]

rightPanel : (Int,Int) -> Entry -> Element
rightPanel (w,h) en =
  let e = Entry.unwrap en
  in flow down (
  [ "⌘P: promote from inbox" |> hintText
  ] ++ indexedMap child e.children)
  |> container w h topLeft

title : (Int,Int) -> Entry -> Element
title (w,h) en = Entry.unwrap en |> .text |> plainText |> container w 30 midLeft |> color red

subtitle : (Int,Int) -> Entry -> Element
subtitle (w,h) en = Entry.unwrap en |> .text |> plainText |> container w 30 midLeft |> color green

footer (w,h) = flow right (map (\x -> asText x) 
  [ "⌘D: delete"
  , "⌘M: Missorted"
  , "⌘Up/Down: move up/down"
  ])
  |> container w 40 midLeft |> color (hsl 0 0 0.8)


render : (Int,Int) -> Document.Zipper -> Element
render (w,h) m =
  let f = footer (w,h)
      header = title (w,h) m.value
      mh = h - (heightOf f) - (heightOf header)
  in flow down
  [ header
  , flow right
    [ leftPanel (toFloat w/2 |> floor,mh) m.value
    , rightPanel (toFloat w/2 |> floor,mh) m.value
    ]
  , f
  ]
