module Outline.RichText.Span.Model (Type(..), Value, Zipper, toValue, value, startZipper, endZipper) where

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

startZipper : Value -> Zipper
startZipper (t,v) = (t, Core.String.startZipper v)

endZipper : Value -> Zipper
endZipper (t,v) = (t, Core.String.endZipper v)
