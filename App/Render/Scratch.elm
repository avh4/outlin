module App.Render.Scratch (render) where

import Core.Array
import Outline.Scratch.Model as Scratch
import App.Render.String as String
import Outline.RichText.Model as RichText
import Outline.RichText.Render as RichText
import App.Render.RichText as RichText
import Outline.RichText.Span.Model as Span
import Outline.RichText.Block.Model as Block
import Outline.RichText.Block.Render as Block
import Rectified (..)
import Signal
import String
import List
import Html

import App.Styles (..)

type Foo = Val Int Scratch.Value | Zip Int Scratch.Zipper

item channel n = case n of
  Zip _ _ -> empty |> grey 80
  Val i v -> (v |> List.head |> Block.spanToHtml |> html margin) |> panel
      |> clickable (Signal.send channel i)

newScratchButton channel = text dim margin "+ New Scratch..."
  |> grey 70
  |> clickable (Signal.send channel ())

navbar : Signal.Channel Int -> Signal.Channel () -> Signal.Channel () -> Core.Array.Zipper Scratch.Value Scratch.Zipper -> Element
navbar scratchChannel processChannel newScratchChannel z = z
  |> Core.Array.indexedMap (\i v -> Val i v) (\i z -> Zip i z)
  |> list 60 2 (item scratchChannel)
  |> top 40 2 (newScratchButton newScratchChannel)

task : Block.Value -> Element
task b = b
  |> Block.spanToHtml
  |> html 0
  |> left 30 0 (text bold 0 ">>>" |> grey 20)

content : Signal.Channel () -> Scratch.Zipper -> Element
content processChannel z =
  let tasks = (z |> RichText.toValue |> RichText.getTasks)
  in
    html margin (RichText.toHtml z)
    |> bottom (20*List.length tasks) margin (list 20 0 task tasks)
    |> bottom 50 margin
      (debug "[PROCESS]" |> grey 60 |> clickable (Signal.send processChannel ()))
    |> panel

render : Signal.Channel Int -> Signal.Channel () -> Signal.Channel () -> Core.Array.Zipper Scratch.Value Scratch.Zipper -> Element
render scratchChannel processChannel newScratchChannel z =
  Core.Array.active z |> content processChannel
  |> left 280 margin (navbar scratchChannel processChannel newScratchChannel z)
  |> inset margin
