module App.Render.Scratch (render) where

import Core.Array
import Outline.Scratch.Model as Scratch
import Graphics.Element (..)
import Text (..)

renderValue : Scratch.Value -> Element
renderValue v = asText <| toString v

renderZipper : Scratch.Zipper -> Element
renderZipper z = renderValue (Scratch.toValue z) -- TODO

render : Core.Array.Zipper Scratch.Value Scratch.Zipper -> Element
render z = flow down (Core.Array.map renderValue renderZipper z)
