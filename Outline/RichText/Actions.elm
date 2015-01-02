module Outline.RichText.Actions
  ( doBlock, doSpan
  ) where

import Outline.RichText.Model (..)
import Outline.RichText.Block.Model as Block
import Outline.RichText.Block.Actions as Block
import Outline.RichText.Span.Actions as Span
import Core.Action (..)
import Core.Array
import RichText (..)
import RichText.SpanZipper (..)

type alias Result = ActionResult Value Zipper

doArrayMaybe : (Zipper -> Maybe Zipper) -> Zipper -> Result
doArrayMaybe fn z = case fn z of
  Just z' -> Update z'
  Nothing -> NoChange

doBlock : (Block.Zipper -> Block.Result) -> Zipper -> Result
doBlock fn z = case fn (Core.Array.active z) of
  Block.Update z' -> Update <| Core.Array.apply (\_ -> z') z
  Block.Split left z' right -> Update <| Core.Array.mergeActive left z' right z
  Block.Join v' -> doArrayMaybe (Core.Array.joinActive Block.mergeZipper v') z
  Block.EnterPrev -> doArrayMaybe (Core.Array.goPrev Block.toValue Block.endZipper) z
  Block.EnterNext -> doArrayMaybe (Core.Array.goNext Block.toValue Block.startZipper) z
  Block.NoChange -> NoChange

-- TODO: on a split, or update, merge adjacent spans with the same type
doSpan : (SpanZipper -> Span.Result) -> Zipper -> Result
doSpan fn = doBlock (Block.doSpan fn)
