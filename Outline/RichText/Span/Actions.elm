module Outline.RichText.Span.Actions (Result, do) where

import Outline.RichText.Span.Model (..)
import Core.Action (..)
import Core.String
import List (map)

type alias Result = ActionResult Value Zipper

do : (Core.String.Zipper -> Core.String.Result) -> Zipper -> Result
do fn (t,s) = case fn s of
  Update z -> Update (t,z)
  Split lefts z rights -> Split (map (\s -> (t,s)) lefts) (t,z) (map (\s -> (t,s)) rights)
  Delete -> Delete
  EnterNext -> EnterNext
  EnterPrev -> EnterPrev
  NoChange -> NoChange
