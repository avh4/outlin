module App.Render.Notes (render) where

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
import Text (plainText, asText)
import Signal
import String
import List
import Html

render : (Int,Int) -> Core.Array.Value RichText.Value -> Element
render (w,h) v =
  List.map (\x -> x |> asText) v
  |> flow down
