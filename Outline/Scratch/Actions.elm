module Outline.Scratch.Actions (Result, do, doText) where

import Outline.Scratch.Model (..)
import Outline.RichText.Span.Model as Span
import Outline.RichText.Span.Actions as Span
import Core.Action (..)
import Core.Array
import Core.String

type alias Result = ActionResult Value Zipper

doText : (Core.String.Zipper -> Core.String.Result) -> Zipper -> Result
doText fn = do (Span.do fn)

do : (Span.Zipper -> Span.Result) -> Zipper -> Result
do fn = Core.Array.do Span.toValue Span.startZipper Span.endZipper fn
