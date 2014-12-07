module Outline.Document.Render (render) where

import Outline.Document.Model (..)
import Core.Action
import Core.Action (..)
import Core.Array
import Outline.Entry as Entry
import Outline.Scratch.Model as Scratch
import Text (asText)
import Graphics.Element (flow, right, down, Element, width, heightOf, widthOf, spacer, color, container, topLeft, midLeft)

render : ((Int,Int) -> Entry.Zipper -> Element) -> (Element -> Element -> Element) -> (Int,Int) -> Zipper -> Element
render outlineFn layoutFn (w,h) z = case z of
  InScratch _ _ -> asText "TBD"
  InOutline sVal eZip -> layoutFn (asText <| toString sVal) (outlineFn (w,h-100) eZip)
