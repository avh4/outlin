module Outline.RichText.Span.Actions (Result, do, applyStyle) where

import Core.Action (..)
import Core.String
import List (map)
import RichText (..)
import RichText.SpanZipper (..)

type alias Result = ActionResult Span SpanZipper

do : (Core.String.Zipper -> Core.String.Result) -> SpanZipper -> Result
do fn (t,s) = case fn s of
  Update z -> Update (t,z)
  Split lefts z rights -> Split (map (\s -> (t,s)) lefts) (t,z) (map (\s -> (t,s)) rights)
  Delete -> Delete
  EnterNext -> EnterNext
  EnterPrev -> EnterPrev
  NoChange -> NoChange

applyStyle : SpanType -> SpanZipper -> (Maybe Span, SpanZipper, Maybe Span)
applyStyle t' (t,sz) = case Core.String.destructure sz of
  (left, sel, right) ->
    ( if left == "" then Nothing else Just (t, left)
    , (t', Core.String.allZipper sel)
    , if right == "" then Nothing else Just (t, right)
    )
