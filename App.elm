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
import Core.Array
import Color (..)
import Text
import Outline.Document as Document
import App.EntryNav as EntryNav

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
  = Key Keys.KeyCombo
  | Loaded String

step : Command -> Document.Zipper -> Document.Zipper
step c m = case c of
  Key (Keys.Single (Keys.Left)) -> updateModel Entry.goLeft m
  Key (Keys.Single (Keys.Right)) -> updateModel Entry.goRight m
  Key (Keys.Single (Keys.Down)) -> updateModel Entry.goNext m
  Key (Keys.Single (Keys.Up)) -> updateModel Entry.goPrev m
  Key (Keys.Single (Keys.Enter)) -> updateModel Entry.enter m
  Key (Keys.Single (Keys.Backspace)) -> updateModel Entry.backspace m
  Key (Keys.Character s) -> updateModel (Entry.insert s) m
  Key (Keys.CommandCharacter "a") -> updateModel Entry.addInboxItem m
  Key (Keys.CommandCharacter "d") -> updateModel Entry.delete m
  Key (Keys.CommandCharacter "p") -> updateModel Entry.promote m
  Key (Keys.CommandCharacter "m") -> updateModel Entry.missort m
  Key (Keys.CommandCharacter "1") -> updateModel (Entry.moveInto 0) m
  Key (Keys.CommandCharacter "2") -> updateModel (Entry.moveInto 1) m
  Key (Keys.CommandCharacter "3") -> updateModel (Entry.moveInto 2) m
  Key (Keys.CommandCharacter "4") -> updateModel (Entry.moveInto 3) m
  Key (Keys.CommandCharacter "5") -> updateModel (Entry.moveInto 4) m
  Key (Keys.CommandCharacter "6") -> updateModel (Entry.moveInto 5) m
  Key (Keys.CommandCharacter "7") -> updateModel (Entry.moveInto 6) m
  Key (Keys.Command (Keys.Up)) -> updateModel Entry.moveChildUp m
  Key (Keys.Command (Keys.Down)) -> updateModel Entry.moveChildDown m
  Key (Keys.Command (Keys.Right)) -> updateModel EntryNav.goToFirstChild m
  Key (Keys.Command (Keys.Left)) -> updateModel EntryNav.goToParent m
  Loaded s -> case Json.Decoder.fromString s `Json.Process.into` Entry.decoder of
    Json.Output.Success doc -> Entry.textZipper doc
    x -> fst (m, Debug.log "Load failed" x)
  x -> fst (m, Debug.log "Unhandled command" x)

---- RENDER

textCursor : (String -> Element) -> Core.String.Zipper -> Element
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

rot : Element -> Element
rot e = collage (heightOf e) (widthOf e) [ e |> toForm |> rotate (degrees 90) ]

crumbView : Int -> (Int,String,Int) -> Element
crumbView h (left,text,right) =
  let n = left + right + 1
      e = plainText text |> width (toFloat h/n |> ceiling)
      w = heightOf e |> min 80
  in flow down
  [ spacer w (toFloat h*left/n |> floor) |> color grey
  , e |> rot |> width w |> color purple
  , spacer w (toFloat h*right/n |> floor) |> color grey
  ]

crumbsPanel : Int -> [(Int,String,Int)] -> Element
crumbsPanel h crumbs = flow right (map (crumbView h) crumbs)

siblingView : Int -> Entry.Value -> Element
siblingView w v = case v of
  Entry.Entry e -> e.text |> plainText |> width w |> color orange

leftPanel' : (Int,Int) -> [Entry.Value] -> [Entry.Value] -> Element -> Element -> [Element] -> Element
leftPanel' (w,h) left right textElement descriptionElement inboxElements = flow down (
  (map (siblingView w) left) ++
  [ textElement |> width w |> color green
  , descriptionElement |> width w |> color yellow
  , "⌘A: add to inbox" |> hintText
  ] ++ inboxElements ++ (map (siblingView w) right))
  |> container w h topLeft

leftPanel : (Int,Int) -> Entry.Zipper -> [Entry.Value] -> [Entry.Value] -> Element
leftPanel size z left right = case z of
  -- TODO: refactor to use a record of functions so that each case only needs to specify the zipper function
  Entry.InText e -> leftPanel' size left right
    (e.text |> textCursor plainText)
    (e.description |> plainText)
    (map inboxItemV e.inbox)
  Entry.InDescription e -> leftPanel' size left right
    (e.text |> plainText)
    (e.description |> textCursor plainText)
    (map inboxItemV e.inbox)
  Entry.InInbox e -> leftPanel' size left right
    (e.text |> plainText)
    (e.description |> plainText)
    (Core.Array.map inboxItemV inboxItem e.inbox)
  _ -> case Entry.toValue z of
    Entry.Entry e -> leftPanel' size left right
      (e.text |> plainText)
      (e.description |> plainText)
      (map inboxItemV e.inbox)

child : Int -> Element -> Element
child i e = flow right
  [ "⌘" ++ (show <| i+1) ++ " " |> hintText
  , e
  ]

rightPanel' : (Int,Int) -> [Element] -> Element
rightPanel' (w,h) childElements = flow down (
  [ "⌘P: promote from inbox" |> hintText
  ] ++ indexedMap child childElements)
  |> container w h topLeft

rightPanel : (Int,Int) -> Entry.Zipper -> Element
rightPanel size z = case z of
  _ -> case Entry.toValue z of
    Entry.Entry e -> rightPanel' size
      (e.children |> map (\en -> case en of Entry.Entry e -> e.text) |> map plainText)

title : (Int,Int) -> String -> Element
title (w,h) s = s |> plainText |> width w |> color red

footer (w,h) = flow right (map (\x -> asText x)
  [ "⌘D: delete"
  , "⌘M: Missorted"
  , "⌘Up/Down: move up/down"
  ])
  |> container w 40 midLeft |> color (hsl 0 0 0.8)

findFocus : (Int,Int) -> ([Entry.Value],[Entry.Value]) -> Entry.Zipper -> (([Entry.Value],Entry.Zipper,[Entry.Value]),[(Int,String,Int)])
findFocus (l,r) (ls,rs) z = case z of
  Entry.InChild e -> case Core.Array.active e.children |> findFocus (Core.Array.countLeft e.children,Core.Array.countRight e.children) (Core.Array.lefts e.children,Core.Array.rights e.children) of
    (result,crumbs) -> (result, (l,e.text,r) :: crumbs)
  _ -> ((ls,z,rs),[])

render : (Int,Int) -> Document.Zipper -> Element
render (w,h) z =
  let f = footer (w,h)
      header = title (w,h) (Entry.textValue z)
      mh = h - (heightOf f) - (heightOf header)
      ((ls,focus,rs),crumbs) = findFocus (0,0) ([],[]) z
      crumbs' = crumbsPanel mh (drop 1 crumbs)
      mw = w - (widthOf crumbs')
  in flow down
  [ header
  , flow right
    [ crumbs'
    , leftPanel (toFloat mw/2 |> floor,mh) focus ls rs
    , rightPanel (toFloat mw/2 |> ceiling,mh) focus
    ]
  , f
  ]
