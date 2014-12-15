module Outline.RichText.Actions (doSpan) where

import Outline.RichText.Model (..)
import Outline.RichText.Span.Model as Span
import Outline.RichText.Span.Actions as Span
import Core.Action (..)
import Core.Array

type alias Result = ActionResult Value Zipper

-- TODO: on a split, or update, merge adjacent spans with the same type
doSpan : (Span.Zipper -> Span.Result) -> Zipper -> Result
doSpan fn = Core.Array.do Span.toValue Span.startZipper Span.endZipper fn
