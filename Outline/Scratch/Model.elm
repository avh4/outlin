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
import RichText (..)

type alias Value = RichText.Value
type alias Zipper = RichText.Zipper

value : String -> Value
value s = [heading s]

toValue = RichText.toValue

endZipper = RichText.endZipper
allZipper = RichText.allZipper
