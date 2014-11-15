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
import Core.Action as Action
import Core.String
import Color (..)
import Text
import Outline.Document as Document

---- App

updateModel : (z -> Action.Result val z) -> z -> z
updateModel action z = case action z of
  Action.Update newZ -> newZ
  -- explicity list the following action results, which are all no-ops on document
  Action.Split _ _ _ -> z
  Action.Delete -> z
  Action.EnterNext -> z
  Action.EnterPrev -> z
  Action.NoChange -> z

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
    Json.Output.Success doc -> Entry.textZipper doc
    x -> fst (m, Debug.log "Load failed" x)
  x -> fst (m, Debug.log "Unhandled command" x)

---- RENDER

textCursor fn z = case Core.String.toTuple z of
  (left,r) -> flow right
    [ fn left
    , plainText "^"
    , fn r
    ]

hintText : String -> Element
hintText s = s |> toText |> Text.italic |> Text.color (hsl 0 0 0.7) |> leftAligned

inboxItem : Entry.Zipper -> Element
inboxItem z = case z of
  Entry.InText e -> textCursor plainText e.text
  _ -> plainText (Entry.textValue z)

inboxItemV : Entry.Value -> Element
inboxItemV v = case v of Entry.Entry e -> plainText (e.text)

leftPanel : (Int,Int) -> Entry.Zipper -> Element
leftPanel (w,h) z = case z of
  -- InText e ->
  -- InInbox e ->
  _ -> case Entry.toValue z of
    Entry.Entry e -> flow down (
      [ subtitle (w,h) e.text
      , subtitle (w,h) e.description |> color yellow
      , "⌘A: add to inbox" |> hintText
      ] ++ map inboxItemV e.inbox)
      |> container w h topLeft

child : Int -> Entry.Value -> Element
child i v = case v of
  Entry.Entry e -> flow right
    [ "⌘" ++ (show <| i+1) ++ " " |> hintText
    , plainText e.text
    ]

rightPanel : (Int,Int) -> Entry.Zipper -> Element
rightPanel (w,h) z = case z of
  -- InChild e ->
  _ -> case Entry.toValue z of
    Entry.Entry e -> flow down (
      [ "⌘P: promote from inbox" |> hintText
      ] ++ indexedMap child e.children)
      |> container w h topLeft

title : (Int,Int) -> String -> Element
title (w,h) s = s |> plainText |> container w 30 midLeft |> color red

subtitle : (Int,Int) -> String -> Element
subtitle (w,h) s = s |> plainText |> container w 30 midLeft |> color green

footer (w,h) = flow right (map (\x -> asText x)
  [ "⌘D: delete"
  , "⌘M: Missorted"
  , "⌘Up/Down: move up/down"
  ])
  |> container w 40 midLeft |> color (hsl 0 0 0.8)


render : (Int,Int) -> Document.Zipper -> Element
render (w,h) z =
  let f = footer (w,h)
      header = title (w,h) (Entry.textValue z)
      mh = h - (heightOf f) - (heightOf header)
  in flow down
  [ header
  , flow right
    [ leftPanel (toFloat w/2 |> floor,mh) z
    , rightPanel (toFloat w/2 |> floor,mh) z
    ]
  , f
  ]
