module Outline.RichText.Span.Model
  ( Type(..), Value, Zipper
  , toValue, toString, value, normal, empty
  , startZipper, endZipper, allZipper, rangeZipper
  ) where

import Core.String
import Core.Array
import Core.Tagged.Model as Tagged

type Type
  = Normal
  | Bold
  | Task
  | Link String

type alias Value = Tagged.Value Type Core.String.Value
type alias Zipper = Tagged.Zipper Type Core.String.Zipper

toValue : Zipper -> Value
toValue = Tagged.toValue Core.String.toValue

toString : Value -> String
toString (t,s) = s

value : Type -> String -> Value
value = Tagged.value

normal : String -> Value
normal = Tagged.value Normal

empty : Value
empty = normal ""

startZipper : Value -> Zipper
startZipper = Tagged.toZipper Core.String.startZipper

endZipper : Value -> Zipper
endZipper = Tagged.toZipper Core.String.endZipper

allZipper : Value -> Zipper
allZipper = Tagged.toZipper Core.String.allZipper

rangeZipper : (Int,Int) -> Value -> Zipper
rangeZipper range = Tagged.toZipper (Core.String.rangeZipper range)
