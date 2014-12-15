module Outline.RichText.Span.Model
  ( Type(..), Value, Zipper
  , toValue, value
  , startZipper, endZipper, allZipper, rangeZipper
  ) where

import Core.String
import Core.Array

type Type
  = Normal
  | Bold
  | Task
  | Link String

type alias Value = (Type, Core.String.Value)
type alias Zipper = (Type, Core.String.Zipper)

toValue : Zipper -> Value
toValue (t,z) = (t, Core.String.toValue z)

value : Type -> String -> Value
value t s = (t, s)

stringZipper : (Core.String.Value -> Core.String.Zipper) -> Value -> Zipper
stringZipper fn (t,v) = (t, fn v)

startZipper : Value -> Zipper
startZipper = stringZipper Core.String.startZipper

endZipper : Value -> Zipper
endZipper = stringZipper Core.String.endZipper

allZipper : Value -> Zipper
allZipper = stringZipper Core.String.allZipper

rangeZipper : (Int,Int) -> Value -> Zipper
rangeZipper range = stringZipper (Core.String.rangeZipper range)
