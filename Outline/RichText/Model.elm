module Outline.RichText.Model
  ( Value, Zipper
  , toValue
  , endZipper, allZipper
  , getTasks
  ) where

import Core.String
import Core.Array
import String
import List
import List ((::))
import Maybe (Maybe(..), withDefault)
import RichText (..)
import RichText.BlockZipper as Block
import RichText.BlockZipper (BlockZipper)

type alias Value = List Block
type alias Zipper = Core.Array.Zipper Block BlockZipper

toValue : Zipper -> Value
toValue = Core.Array.toValue Block.toValue

endZipper : Value -> Zipper
endZipper v = Core.Array.lastZipperOr (paragraph "") Block.endZipper v

allZipper : Value -> Zipper
allZipper v = Core.Array.lastZipperOr (paragraph "") Block.allZipper v

getTasks : Value -> List Block
getTasks v = List.filter (\(t,s) -> t == Task) v
