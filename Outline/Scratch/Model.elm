module Outline.Scratch.Model where

import Core.Action
import Core.Array
import Core.String
import Outline.RichText.Model as RichText

type alias Value = RichText.Value
type alias Zipper = RichText.Zipper

toValue = RichText.toValue

endZipper = RichText.endZipper
