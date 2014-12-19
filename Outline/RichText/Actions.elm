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
doBlock fn = Core.Array.do Block.toValue Block.startZipper Block.endZipper fn

-- TODO: on a split, or update, merge adjacent spans with the same type
doSpan : (Span.Zipper -> Span.Result) -> Zipper -> Result
doSpan fn = doBlock (Block.doSpan fn)
