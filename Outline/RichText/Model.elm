module Outline.RichText.Model
  ( Value, Zipper
  , value
  , toValue, split, filter
  , endZipper
  ) where

import Core.String
import Core.Array
import Outline.RichText.Span.Model as Span
import Outline.RichText.Span.Json as Span
import String
import List
import List ((::))

type alias Value = Core.Array.Value Span.Value
type alias Zipper = Core.Array.Zipper Span.Value Span.Zipper

value : String -> Value
value s = [(Span.Normal, s)]

toValue : Zipper -> Value
toValue = Core.Array.toValue Span.toValue

endZipper : Value -> Zipper
endZipper v = case Core.Array.lastZipperM Span.endZipper v of
  Just z -> z
  Nothing -> Core.Array.zipper [] (Span.endZipper <| Span.value Span.Normal "") []

splitOne : String -> Span.Value -> List Span.Value
splitOne needle (t,s) = String.split needle s |> List.map (\s -> (t,s))

splitAppend_ : List a -> (List a, List (List a)) -> (List a, List (List a))
splitAppend_ next (acc,finished) = case next of
  [] -> (acc,finished)
  (single::[]) -> (single::acc,finished)
  (first::rest) -> case List.reverse rest of
    (last::mid) -> ([last], (List.map (\x -> [x]) mid) ++ ((List.reverse (first::acc))::finished))

split : String -> Value -> List Value
split needle v = List.map (splitOne needle) v
  |> List.foldl splitAppend_ ([],[])
  |> (\(acc,finished) -> List.reverse (List.reverse acc::finished))

filter : (Span.Type -> String -> Bool) -> Zipper -> List Span.Value
filter predicate z = z
  |> Core.Array.map identity Span.toValue
  |> List.filter (\(t,s) -> predicate t s)
