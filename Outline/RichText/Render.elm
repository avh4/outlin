module Outline.RichText.Render
  ( toHtml
  ) where

import Outline.RichText.Block.Render as Block
import Outline.RichText.Model (..)
import Html (..)
import List
import Core.Array
import App.Render.String as String

toHtml : Zipper -> Html
toHtml z = z
  |> Core.Array.map Block.valueToHtml Block.zipperToHtml
  |> node "div" []
