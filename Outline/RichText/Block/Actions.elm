module Outline.RichText.Block.Actions
  ( Result
  , doSpan
  , toggleStyle, split
  ) where

import Outline.RichText.Block.Model (..)
import Core.Action (..)
import Core.Array
import Core.String
import Outline.RichText.Span.Model as Span
import Outline.RichText.Span.Actions as Span
import List (map)

type alias Result = ActionResult Value Zipper

doSpan : (Span.Zipper -> Span.Result) -> Zipper -> Result
doSpan spanFn (t,z) = case Core.Array.doPropagatingSplits Span.toValue Span.startZipper Span.endZipper spanFn z of
  Update z -> Update (t,z)
  Split lefts z rights -> Split (map (\s -> (t,s)) lefts) (t,z) (map (\s -> (t,s)) rights)
  Delete -> Delete
  EnterNext -> EnterNext
  EnterPrev -> EnterPrev
  NoChange -> NoChange

toggle : Type -> Type -> Type
toggle current t = if current == t then Paragraph else t

toggleStyle : Type -> Zipper -> Result
toggleStyle t' (t,sz) = Update (toggle t t',sz) -- TODO

split : Zipper -> Result
split (t, z) = case Core.Array.doPropagatingSplits Span.toValue Span.startZipper Span.endZipper (Span.do Core.String.split) z of
  Update z -> Update (t,z)
  Split lefts z rights -> Split (map (\s -> (t,s)) lefts) (t,z) (map (\s -> (t,s)) rights)
  Delete -> Delete
  EnterNext -> EnterNext
  EnterPrev -> EnterPrev
  NoChange -> NoChange
