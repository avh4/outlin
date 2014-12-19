module Outline.RichText.Actions
  ( doBlock, doSpan
  ) where

import Outline.RichText.Model (..)
import Outline.RichText.Block.Model as Block
import Outline.RichText.Block.Actions as Block
import Outline.RichText.Span.Model as Span
import Outline.RichText.Span.Actions as Span
import Core.Action (..)
import Core.Array

type alias Result = ActionResult Value Zipper

doBlock : (Block.Zipper -> Block.Result) -> Zipper -> Result
doBlock fn z = case fn (Core.Array.active z) of
  Block.Update z' -> Update <| Core.Array.replaceActive z' z
  Block.Split left z' right -> Update <| Core.Array.mergeActive left z' right z
  Block.Join v' -> case Core.Array.joinActive Block.mergeZipper v' z of
    Nothing -> NoChange
    Just z'' -> Update z''
  Block.NoChange -> NoChange
  Block.EnterPrev -> NoChange
  Block.EnterNext -> NoChange

-- TODO: on a split, or update, merge adjacent spans with the same type
doSpan : (Span.Zipper -> Span.Result) -> Zipper -> Result
doSpan fn = doBlock (Block.doSpan fn)
