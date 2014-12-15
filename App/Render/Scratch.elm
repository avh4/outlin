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

navItem : Signal.Channel Int -> Int -> Scratch.Value -> Element
navItem channel i v = v
  |> RichText.split "\n" |> List.head
  |> RichText.toElement 200 40
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
renderZipper z = RichText.render z

render : Signal.Channel Int -> Core.Array.Zipper Scratch.Value Scratch.Zipper -> Element
render scratchChannel z = flow right
  [ list scratchChannel z
  , Core.Array.active z |> renderZipper
  ]
