module Outline.RichText.Model
  ( Value, Zipper
  , value, heading
  , toValue
  , endZipper, allZipper
  , getTasks
  ) where

import Core.String
import Core.Array
import Outline.RichText.Block.Model as Block
import String
import List
import List ((::))
import Maybe (Maybe(..), withDefault)
import RichText

type alias Value = List Block.Value
type alias Zipper = Core.Array.Zipper Block.Value Block.Zipper

value : String -> Value
value s = s |> String.split "\n"
  |> List.map (\s -> s |> RichText.span |> Block.paragraph)

heading : String -> Value
heading s = s |> String.split "\n"
  |> List.map (\s -> Block.value Block.Heading [s |> RichText.span])

toValue : Zipper -> Value
toValue = Core.Array.toValue Block.toValue

emptyZipper : Zipper
emptyZipper = Core.Array.zipper [] (Block.endZipper <| Block.paragraph <| RichText.span "") []

endZipper : Value -> Zipper
endZipper v = Core.Array.lastZipperM Block.endZipper v
  |> withDefault emptyZipper

allZipper : Value -> Zipper
allZipper v = Core.Array.lastZipperM Block.allZipper v
  |> withDefault emptyZipper

getTasks : Value -> List Block.Value
getTasks v = v
  |> List.filter (\(t,s) -> t == Block.Task)
