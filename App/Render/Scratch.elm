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
import Graphics.Element (..)
import Graphics.Input (clickable)
import Color (..)
import Text (plainText)
import Signal
import String
import List
import Html

navItem : Int -> Signal.Channel Int -> Int -> Scratch.Value -> Element
navItem w channel i v = v
  |> List.head
  |> Block.spanToHtml |> Html.toElement w 40
  |> clickable (Signal.send channel i)

selectedNavItem : Int -> Signal.Channel Int -> Int -> Scratch.Zipper -> Element
selectedNavItem w channel i z = z
  |> Scratch.toValue
  |> navItem w channel i
  |> color yellow

list w channel z = z
  |> Core.Array.indexedMap (navItem w channel) (selectedNavItem w channel)
  |> flow down

task : Int -> Block.Value -> Element
task w (_,s) = flow right
  [ plainText ">>> " |> color blue
  , plainText (toString s)
  ]
  |> width w

tasks : Int -> Scratch.Zipper -> Element
tasks w z = z
  |> RichText.toValue
  |> RichText.getTasks
  |> List.map (task w)
  |> flow down

renderZipper : Int -> Signal.Channel () -> Scratch.Zipper -> Element
renderZipper w processChannel z = flow down
  [ RichText.render w z
  , tasks w z
  , plainText "[PROCESS]" |> color red |> clickable (Signal.send processChannel ())
  ]

render : Int -> Signal.Channel Int -> Signal.Channel () -> Core.Array.Zipper Scratch.Value Scratch.Zipper -> Element
render w scratchChannel processChannel z = flow right
  [ list 200 scratchChannel z
  , Core.Array.active z |> renderZipper (w-200) processChannel
  ]
