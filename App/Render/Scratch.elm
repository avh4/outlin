module App.Render.Scratch (render) where

import Core.Array
import Outline.Scratch.Model as Scratch
import App.Render.String as String
import Graphics.Element (..)
import Graphics.Input (clickable)
import Color (..)
import Text (plainText)
import Signal
import String
import List

navItem : Signal.Channel Int -> Int -> Scratch.Value -> Element
navItem channel i v = v
  |> String.split "\n" |> List.head
  |> plainText
  |> width 200
  |> height 40
  |> clickable (Signal.send channel i)

selectedNavItem : Signal.Channel Int -> Int -> Scratch.Zipper -> Element
selectedNavItem channel i z = z
  |> Scratch.toValue
  |> navItem channel i
  |> color yellow

list channel z = z
  |> Core.Array.indexedMap (navItem channel) (selectedNavItem channel)
  |> flow down

renderZipper : Scratch.Zipper -> Element
renderZipper z = String.render plainText z

render : Signal.Channel Int -> Core.Array.Zipper Scratch.Value Scratch.Zipper -> Element
render scratchChannel z = flow right
  [ list scratchChannel z
  , Core.Array.active z |> renderZipper
  ]
