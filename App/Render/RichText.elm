module App.Render.RichText (render) where

import Core.String
import Graphics.Element (..)
import Html (..)
import Html.Attributes (..)
import String
import List
import Core.Array
import Outline.RichText.Model as RichText
import Outline.RichText.Render as RichText
import Outline.RichText.Span.Model as Span
import Outline.RichText.Span.Model (Type(..))
import App.Render.String as String

render : Int -> RichText.Zipper -> Element
render w z = RichText.toHtml z
  |> toElement w 100
