module Outline.Scratch.Actions
  ( Result
  , doBlock, doSpan, doText
  ) where

import Outline.Scratch.Model (..)
import Outline.RichText.Actions as RichText
import Outline.RichText.Span.Actions as Span
import Outline.RichText.Block.Actions as Block
import Core.Action (..)
import Core.Array
import Core.String
import RichText (..)
import RichText.SpanZipper (..)
import RichText.BlockZipper (..)

type alias Result = ActionResult Value Zipper

doBlock : (BlockZipper -> Block.Result) -> Zipper -> Result
doBlock = RichText.doBlock

doSpan : (SpanZipper -> Span.Result) -> Zipper -> Result
doSpan = RichText.doSpan

doText : (Core.String.Zipper -> Core.String.Result) -> Zipper -> Result
doText fn = doSpan (Span.do fn)
