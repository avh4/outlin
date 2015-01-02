module Outline.RichText.Span.Actions (Result, do, applyStyle) where

import Core.Action (..)
import Core.String
import List (map)
import RichText (..)

type alias Result = ActionResult Span SpanZipper

do : (Core.String.Zipper -> Core.String.Result) -> SpanZipper -> Result
do fn (t,s) = case fn s of
  Update z -> Update (t,z)
  Split lefts z rights -> Split (map (\s -> (t,s)) lefts) (t,z) (map (\s -> (t,s)) rights)
  Delete -> Delete
  EnterNext -> EnterNext
  EnterPrev -> EnterPrev
  NoChange -> NoChange

-- TODO: should just return a zipper
applyStyle : SpanType -> SpanZipper -> Result
applyStyle t' (t,sz) = case Core.String.destructure sz of
  (_, "", _) -> NoChange
  ("", _, "") -> Update (t',sz)
  (left, sel, right) -> Split
    (if left == "" then [] else [(t, left)])
    (t', Core.String.allZipper sel)
    (if right == "" then [] else [(t, right)])
