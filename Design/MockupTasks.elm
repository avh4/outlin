module Main where

import Rectified (..)
import Signal
import Window
import List
import Text
import Color as C
import Graphics.Element as G
import Html

margin = 8

highlight = grey 70
background = grey 85
panel = grey 98

plain = Text.defaultStyle
bold =  { plain | bold <- True }

tab (isHighlight, string) = case isHighlight of
  True -> centeredText bold string |> highlight
  False -> centeredText plain string

tabbar = row 0 tab [(False,"Scratch"), (True,"Tasks"), (False,"Notes")]
  |> panel

item n = case n of
  3 -> (centeredText plain <| "Item " ++ toString n) |> highlight
  _ -> (centeredText plain <| "Item " ++ toString n) |> panel

navbar = row 2 item [1..3]
  |> rotateLeft

todoExample = empty
  |> left 300 2 (crumb "x")
  |> left 300 2 (crumb "b")

crumb string = empty
  |> top 60 0 (text plain margin string)
  |> panel

sortTask =
  Html.node "ul" []
    [ Html.node "li" [] [ Html.text "Buy milk" ]
    , Html.node "li" [] [ Html.text "XX" ]
    ]
  |> html margin
  |> panel
  |> top 60 2 (text plain margin "Daily" |> panel)

sortTarget = list 20 margin (text plain 0) ["Chores", "Work"]
  |> inset margin
  |> panel

sortingExample = empty
  |> left 200 2 (sortTarget)
  |> left 200 2 (sortTask)
  |> left 200 2 (crumb "By Time")

-- tasks (w,h) = (empty
--   |> top 300 margin todoExample
--   |> top 200 margin sortingExample) (w,h)

tasks (w,h) =
  let at x y =  G.container 1000 1000 (G.topLeftAt (G.absolute x) (G.absolute y))
  in G.flow G.inward
    [ (crumb "By Time" |> grey 93) (200, 416) |> at 0 0
    , (crumb "Daily" |> grey 93) (200, 200) |> at 208 0
    , (debug "inbox" |> panel) (200, 200) |> at 416 0
    , (debug "children" |> panel) (200, 200) |> at 624 0
    , (crumb "Monthly" |> grey 87) (200, 200) |> at 208 216
    , (crumb "review finances" |> grey 87) (200, 200) |> at 416 216
    ]
    |> G.scrollContainer w h


scene = tasks
  |> inset margin
  |> top 60 0 tabbar
  |> background


main = Signal.map scene Window.dimensions
