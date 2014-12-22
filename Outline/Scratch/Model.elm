module Outline.Scratch.Model
  ( Value, Zipper
  , value
  , toValue
  , endZipper, allZipper
  ) where

import Core.Action
import Core.Array
import Core.String
import Outline.RichText.Model as RichText
import Outline.RichText.Span.Model as Span
import Outline.RichText.Block.Model as Block

type alias Value = RichText.Value
type alias Zipper = RichText.Zipper

value : String -> Value
value s = s
  |> Span.normal |> Core.Array.single
  |> Block.value Block.Heading |> Core.Array.single

toValue = RichText.toValue

endZipper = RichText.endZipper
allZipper = RichText.allZipper
