module App.Render.Outline (render) where

import Core.String
import Core.Array
import Outline.Entry as Entry
import List
import List ((::))
import Graphics.Element (..)
import Graphics.Collage (..)
import Color (..)
import Text
import Text (plainText)

textCursor : (String -> Element) -> Core.String.Zipper -> Element
textCursor fn z = case Core.String.toTuple z of
  (left,r) -> flow right
    [ fn left
    , plainText "^"
    , fn r
    ]

hintText : String -> Element
hintText s = s |> Text.fromString |> Text.italic |> Text.color (hsl 0 0 0.7) |> Text.leftAligned

inboxItem : Int -> Entry.Zipper -> Element
inboxItem w z = case z of
  Entry.InText e -> textCursor plainText e.text |> width w
  _ -> plainText (Entry.textValue z) |> width w

inboxItemV : Int -> Entry.Value -> Element
inboxItemV w v = case v of Entry.Entry e -> plainText (e.text) |> width w

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

crumbsPanel : Int -> List (Int,String,Int) -> Element
crumbsPanel h crumbs = flow right (List.map (crumbView h) crumbs)

siblingView : Int -> Entry.Value -> Element
siblingView w v = case v of
  Entry.Entry e -> e.text |> plainText |> width w |> color orange

leftPanel' : (Int,Int) -> List Entry.Value -> List Entry.Value -> Element -> Element -> List Element -> Element
leftPanel' (w,h) left right textElement descriptionElement inboxElements = flow down (
  (List.map (siblingView w) left) ++
  [ textElement |> width w |> color green
  , descriptionElement |> width w |> color yellow
  , "⌘A: add to inbox" |> hintText
  ] ++ inboxElements ++ (List.map (siblingView w) right))
  |> container w h topLeft

leftPanel : (Int,Int) -> Entry.Zipper -> List Entry.Value -> List Entry.Value -> Element
leftPanel (w,h) z left right = case z of
  -- TODO: refactor to use a record of functions so that each case only needs to specify the zipper function
  Entry.InText e -> leftPanel' (w,h) left right
    (e.text |> textCursor plainText)
    (e.description |> plainText)
    (List.map (inboxItemV w) e.inbox)
  Entry.InDescription e -> leftPanel' (w,h) left right
    (e.text |> plainText)
    (e.description |> textCursor plainText)
    (List.map (inboxItemV w) e.inbox)
  Entry.InInbox e -> leftPanel' (w,h) left right
    (e.text |> plainText)
    (e.description |> plainText)
    (Core.Array.map (inboxItemV w) (inboxItem w) e.inbox)
  _ -> case Entry.toValue z of
    Entry.Entry e -> leftPanel' (w,h) left right
      (e.text |> plainText)
      (e.description |> plainText)
      (List.map (inboxItemV w) e.inbox)

child : Int -> Element -> Element
child i e = flow right
  [ "⌘" ++ (toString <| i+1) ++ " " |> hintText
  , e
  ]

rightPanel' : (Int,Int) -> List Element -> Element
rightPanel' (w,h) childElements = flow down (
  [ "⌘P: promote from inbox" |> hintText
  ] ++ List.indexedMap child childElements)
  |> container w h topLeft

rightPanel : (Int,Int) -> Entry.Zipper -> Element
rightPanel size z = case z of
  _ -> case Entry.toValue z of
    Entry.Entry e -> rightPanel' size
      (e.children |> List.map (\en -> case en of Entry.Entry e -> e.text) |> List.map plainText)

title : (Int,Int) -> String -> Element
title (w,h) s = s |> plainText |> width w |> color red

footer (w,h) = flow right (List.map (\x -> plainText (x ++ "    "))
  [ "⌘D: delete"
  , "⌘M: Missorted"
  , "⌘Up/Down: move up/down"
  , "Shift-Up/Down/Left/Right: navigate hierarchy"
  ])
  |> container w 40 midLeft |> color (hsl 0 0 0.8)

findFocus : (Int,Int) -> (List Entry.Value,List Entry.Value) -> Entry.Zipper -> ((List Entry.Value,Entry.Zipper,List Entry.Value),List (Int,String,Int))
findFocus (l,r) (ls,rs) z = case z of
  Entry.InChild e -> case Core.Array.active e.children |> findFocus (Core.Array.countLeft e.children,Core.Array.countRight e.children) (Core.Array.lefts e.children,Core.Array.rights e.children) of
    (result,crumbs) -> (result, (l,e.text,r) :: crumbs)
  _ -> ((ls,z,rs),[])

render : (Int,Int) -> Entry.Zipper -> Element
render (w,h) z =
  let f = footer (w,h)
      header = title (w,h) (Entry.textValue z)
      mh = h - (heightOf f) - (heightOf header)
      ((ls,focus,rs),crumbs) = findFocus (0,0) ([],[]) z
      crumbs' = crumbsPanel mh (List.drop 1 crumbs)
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
