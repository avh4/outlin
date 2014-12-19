module Outline.RichText.Block.Model
  ( Type(..), Value, Zipper
  , toValue, toString, paragraph, value, empty
  , mergeZipper
  , startZipper, endZipper, allZipper
  ) where

-- A RichText.Block is a list of tagged RichText.Spans

import Core.Tagged.Model as Tagged
import Core.Array
import Outline.RichText.Span.Model as Span
import String
import List

type Type
  = Paragraph
  | Quote
  | Task

type alias Value = Tagged.Value Type (Core.Array.Value Span.Value)
type alias Zipper = Tagged.Zipper Type (Core.Array.Zipper Span.Value Span.Zipper)

empty : Value
empty = paragraph Span.empty

value : Type -> List Span.Value -> Value
value = Tagged.value

paragraph : Span.Value -> Value
paragraph span = Tagged.value Paragraph [span]

toValue : Zipper -> Value
toValue = Tagged.toValue (Core.Array.toValue Span.toValue)

toString : Value -> String
toString (t,v) = String.join "" (List.map Span.toString v)

mergeZipper : Value -> Value -> Zipper
mergeZipper (t1,spans1) (t2,spans2) = case spans2 of
  [] -> (t1,spans1) |> endZipper
  (head::rest) -> (t1, Core.Array.zipper spans1 (head |> Span.startZipper) rest)

j : (a -> b) -> (b -> c) -> a -> c
j f1 f2 v = f1 v |> f2

withDefault : a -> Maybe a -> a
withDefault default maybe =
  case maybe of
    Just value -> value
    Nothing -> default

firstZipperOr : (v -> z) -> z -> Core.Array.Value v -> Core.Array.Zipper v z
firstZipperOr fn default = Core.Array.firstZipperM fn `j` withDefault (Core.Array.zipper [] default [])

lastZipperOr : (v -> z) -> z -> Core.Array.Value v -> Core.Array.Zipper v z
lastZipperOr fn default = Core.Array.lastZipperM fn `j` withDefault (Core.Array.zipper [] default [])

startZipper : Value -> Zipper
startZipper = Tagged.toZipper (firstZipperOr Span.startZipper (Span.empty |> Span.startZipper))

endZipper : Value -> Zipper
endZipper = Tagged.toZipper (lastZipperOr Span.endZipper (Span.empty |> Span.startZipper))

allZipper : Value -> Zipper
allZipper = Tagged.toZipper (lastZipperOr Span.allZipper (Span.empty |> Span.allZipper))
