module Outline.Scratch.Actions
  ( Result
  , doSpan, doText
  ) where

import Outline.Scratch.Model (..)
import Outline.RichText.Actions as RichText
import Outline.RichText.Span.Model as Span
import Outline.RichText.Span.Actions as Span
import Core.Action (..)
import Core.Array
import Core.String

type alias Result = ActionResult Value Zipper

doSpan : (Span.Zipper -> Span.Result) -> Zipper -> Result
doSpan = RichText.doSpan

doText : (Core.String.Zipper -> Core.String.Result) -> Zipper -> Result
doText fn = doSpan (Span.do fn)
