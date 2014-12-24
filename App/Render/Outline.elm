module App.Render.Outline (render) where

import Core.String
import Core.Array
import Outline.Entry as Entry
import App.Render.String as String
import List
import List ((::))
import Rectified (..)
import Text
import Color as C

import App.Styles (..)

type Foo = Val Entry.Value | Zip Entry.Zipper

inboxItem : Foo -> Element
inboxItem z = case z of
  Val (Entry.Entry e) -> e.text |> text plain 0
  Zip (Entry.InText e) -> e.text |> String.toHtml |> html 0
  Zip z -> Entry.textValue z |> text plain 0

inboxItemV : Entry.Value -> Element
inboxItemV v = inboxItem (Val v)

crumbView : (Int,String,Int) -> Element
crumbView (l,string,r) =
  text plain 0 string |> grey 60
  |> left 20 0 empty
  |> right 20 0 empty
  |> rotateLeft
  -- let n = left + right + 1
  --     e = plainText text |> width (toFloat h/n |> ceiling)
  --     w = heightOf e |> min 80
  -- in flow down
  -- [ spacer w (toFloat h*left/n |> floor) |> color grey
  -- , e |> rot |> width w |> color purple
  -- , spacer w (toFloat h*right/n |> floor) |> color grey
  -- ]

crumbsPanel : List (Int,String,Int) -> Element
crumbsPanel = row 0 crumbView

siblingView : Entry.Value -> Element
siblingView v = case v of
  Entry.Entry e -> e.text |> text plain 0 |> grey 80

leftPanel' : List Entry.Value -> List Entry.Value -> Element -> Element -> List Foo -> Element
leftPanel' l r textElement descriptionElement inbox =
  (list 30 0 inboxItem inbox)
  |> top 30 0 (text dim 0 "⌘A: add to inbox")
  |> top 60 0 descriptionElement
  |> top 50 0 textElement
  |> top (20*List.length l) margin (list 20 0 siblingView l)
  |> bottom (20*List.length r) margin (list 20 0 siblingView r)

leftPanel : Entry.Zipper -> List Entry.Value -> List Entry.Value -> Element
leftPanel z left right = case z of
  -- TODO: refactor to use a record of functions so that each case only needs to specify the zipper function
  Entry.InText e -> leftPanel' left right
    (e.text |> String.toHtml |> html 0)
    (e.description |> text plain 0)
    (e.inbox |> List.map Val)
  Entry.InDescription e -> leftPanel' left right
    (e.text |> text plain 0)
    (e.description |> String.toHtml |> html 0)
    (e.inbox |> List.map Val)
  Entry.InInbox e -> leftPanel' left right
    (e.text |> text plain 0)
    (e.description |> text plain 0)
    (Core.Array.map Val Zip e.inbox)
  _ -> case Entry.toValue z of
    Entry.Entry e -> leftPanel' left right
      (e.text |> text plain 0)
      (e.description |> text plain 0)
      (e.inbox |> List.map Val)

child : (Int,String) -> Element
child (i,t) =
  text plain 0 t
  |> left 30 0 (text hint 0 <| "⌘" ++ (toString <| i+1))

rightPanel : Entry.Zipper -> Element
rightPanel z = case Entry.toValue z of
  Entry.Entry e ->
    e.children
    |> List.map (\en -> case en of Entry.Entry e -> e.text)
    |> List.indexedMap (,)
    |> list 30 0 child

footer = row 0 (text dim margin)
  [ "⌘D: delete"
  , "⌘M: Missorted"
  , "⌘Up/Down: move up/down"
  , "Shift-Up/Down/Left/Right: navigate hierarchy"
  ]
  |> grey 90

findFocus : (Int,Int) -> (List Entry.Value,List Entry.Value) -> Entry.Zipper -> ((List Entry.Value,Entry.Zipper,List Entry.Value),List (Int,String,Int))
findFocus (l,r) (ls,rs) z = case z of
  Entry.InChild e -> case Core.Array.active e.children |> findFocus (Core.Array.countLeft e.children,Core.Array.countRight e.children) (Core.Array.lefts e.children,Core.Array.rights e.children) of
    (result,crumbs) -> (result, (l,e.text,r) :: crumbs)
  _ -> ((ls,z,rs),[])

header z = text plain margin (Entry.textValue z) |> grey 70

render : Entry.Zipper -> Element
render z =
  let ((ls,focus,rs),crumbs) = findFocus (0,0) ([],[]) z
  in
    (rightPanel focus)
    |> left 200 0 (leftPanel focus ls rs)
    |> left (50*List.length crumbs) margin (crumbsPanel crumbs)
    |> bottom 40 0 (footer)
    |> top 40 0 (header z)
