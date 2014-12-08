module Outline.Scratch.Model where

import Core.Action
import Core.Array
import Core.String

type alias Value = Core.String.Value
type alias Zipper = Core.String.Zipper

toValue : Zipper -> Value
toValue = Core.String.toValue

endZipper = Core.String.endZipper
