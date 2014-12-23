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
import Rectified (..)
import Graphics.Input (clickable)
import Color (..)
import Text (plainText, asText)
import Signal
import String
import List
import Html

render : Core.Array.Value RichText.Value -> Element
render v = list 80 2 debug v
