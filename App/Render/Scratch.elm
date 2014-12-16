module App.Render.Scratch (render) where

import Core.Array
import Outline.Scratch.Model as Scratch
import App.Render.String as String
import Outline.RichText.Model as RichText
import Outline.RichText.Render as RichText
import App.Render.RichText as RichText
import Graphics.Element (..)
import Graphics.Input (clickable)
import Color (..)
import Text (plainText)
import Signal
import String
import List

navItem : Int -> Signal.Channel Int -> Int -> Scratch.Value -> Element
navItem w channel i v = v
  |> RichText.split "\n" |> List.head
  |> RichText.toElement w 40
  |> clickable (Signal.send channel i)

selectedNavItem : Int -> Signal.Channel Int -> Int -> Scratch.Zipper -> Element
selectedNavItem w channel i z = z
  |> Scratch.toValue
  |> navItem w channel i
  |> color yellow

list w channel z = z
  |> Core.Array.indexedMap (navItem w channel) (selectedNavItem w channel)
  |> flow down

renderZipper : Int -> Scratch.Zipper -> Element
renderZipper w z = RichText.render w z

render : Int -> Signal.Channel Int -> Core.Array.Zipper Scratch.Value Scratch.Zipper -> Element
render w scratchChannel z = flow right
  [ list 200 scratchChannel z
  , Core.Array.active z |> renderZipper (w-200)
  ]
