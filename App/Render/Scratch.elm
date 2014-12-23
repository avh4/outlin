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
  Zip _ _ -> empty
  Val i v -> (v |> List.head |> Block.spanToHtml |> html margin) |> panel
      |> clickable (Signal.send channel i)

navbar : Signal.Channel Int -> Signal.Channel () -> Core.Array.Zipper Scratch.Value Scratch.Zipper -> Element
navbar scratchChannel processChannel z = z
  |> Core.Array.indexedMap (\i v -> Val i v) (\i z -> Zip i z)
  |> list 60 2 (item scratchChannel)

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

render : Signal.Channel Int -> Signal.Channel () -> Core.Array.Zipper Scratch.Value Scratch.Zipper -> Element
render scratchChannel processChannel z =
  left 280 margin (navbar scratchChannel processChannel z) (content processChannel <| Core.Array.active z) |> inset margin
