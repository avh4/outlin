module Outline.RichText.Model
  ( Value, Zipper
  , value, heading
  , toValue
  , endZipper, allZipper
  , getTasks
  ) where

import Core.String
import Core.Array
import Outline.RichText.Span.Model as Span
import Outline.RichText.Span.Json as Span
import Outline.RichText.Block.Model as Block
import String
import List
import List ((::))
import Maybe (Maybe(..), withDefault)

type alias Value = Core.Array.Value Block.Value
type alias Zipper = Core.Array.Zipper Block.Value Block.Zipper

value : String -> Value
value s = s |> String.split "\n"
  |> List.map (\s -> s |> Span.normal |> Block.paragraph)

heading : String -> Value
heading s = s |> String.split "\n"
  |> List.map (\s -> Block.value Block.Heading [s |> Span.normal])

toValue : Zipper -> Value
toValue = Core.Array.toValue Block.toValue

emptyZipper : Zipper
emptyZipper = Core.Array.zipper [] (Block.endZipper <| Block.paragraph <| Span.normal "") []

endZipper : Value -> Zipper
endZipper v = Core.Array.lastZipperM Block.endZipper v
  |> withDefault emptyZipper

allZipper : Value -> Zipper
allZipper v = Core.Array.lastZipperM Block.allZipper v
  |> withDefault emptyZipper

getTasks : Value -> List Block.Value
getTasks v = v
  |> List.filter (\(t,s) -> t == Block.Task)
