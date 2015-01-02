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
import Outline.RichText.Block.Model as Block
import RichText (..)

type alias Value = RichText.Value
type alias Zipper = RichText.Zipper

value : String -> Value
value s = s
  |> span |> (\v -> [v])
  |> Block.value Block.Heading |> (\v -> [v])

toValue = RichText.toValue

endZipper = RichText.endZipper
allZipper = RichText.allZipper
