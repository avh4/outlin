module App.Render.Scratch (render) where

import Core.Array
import Outline.Scratch.Model as Scratch
import App.Render.String as String
import Outline.RichText.Model as RichText
import Outline.RichText.Render as RichText
import App.Render.RichText as RichText
import Outline.RichText.Block.Render as Block
import Rectified (..)
import Signal
import String
import List
import Html
import RichText (Block)

import App.Styles (..)
import App.Command (..)

type Foo = Val Int Scratch.Value | Zip Int Scratch.Zipper

item channel n = case n of
  Zip _ _ -> empty |> grey 80
  Val i v -> (v |> List.head |> Block.spanToHtml |> html margin) |> panel
      |> clickable (Signal.send channel (Scratch i))

newScratchButton channel = text dim margin "+ New Scratch..."
  |> grey 70
  |> clickable (Signal.send channel NewScratch)

navbar : Signal.Channel Command -> Core.Array.Zipper Scratch.Value Scratch.Zipper -> Element
navbar channel z = z
  |> Core.Array.indexedMap (\i v -> Val i v) (\i z -> Zip i z)
  |> list 60 2 (item channel)
  |> top 40 2 (newScratchButton channel)

task : Block -> Element
task b = b
  |> Block.spanToHtml
  |> html 0
  |> left 30 0 (text bold 0 ">>>" |> grey 20)

content : Signal.Channel Command -> Scratch.Zipper -> Element
content channel z =
  let tasks = (z |> RichText.toValue |> RichText.getTasks)
  in
    html margin (RichText.toHtml z)
    |> bottom (20*List.length tasks) margin (list 20 0 task tasks)
    |> bottom 50 margin
      (debug "[PROCESS]" |> grey 60 |> clickable (Signal.send channel ProcessScratch))
    |> panel

render : Signal.Channel Command -> Core.Array.Zipper Scratch.Value Scratch.Zipper -> Element
render channel z =
  Core.Array.active z |> content channel
  |> left 280 margin (navbar channel z)
  |> inset margin
