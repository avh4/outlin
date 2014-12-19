module Outline.RichText.Block.Actions
  ( Result(..)
  , doSpan
  , toggleStyle, split, backspace
  ) where

import Outline.RichText.Block.Model (..)
import Core.Array
import Core.String
import Core.Action as Span
import Outline.RichText.Span.Model as Span
import Outline.RichText.Span.Actions as Span
import List (map)

type Result
  = Update Zipper
  | Split (List Value) Zipper (List Value)
  | Join Value
  | Delete
  | EnterPrev | EnterNext
  | NoChange

doSpan : (Span.Zipper -> Span.Result) -> Zipper -> Result
doSpan spanFn (t,z) = case Core.Array.doPropagatingSplits Span.toValue Span.startZipper Span.endZipper spanFn z of
  Span.Update z -> Update (t,z)
  Span.Split lefts z rights -> Split (map (\s -> (t,s)) lefts) (t,z) (map (\s -> (t,s)) rights)
  Span.Delete -> Delete
  Span.EnterNext -> EnterNext
  Span.EnterPrev -> EnterPrev
  Span.NoChange -> NoChange

toggle : Type -> Type -> Type
toggle current t = if current == t then Paragraph else t

toggleStyle : Type -> Zipper -> Result
toggleStyle t' (t,sz) = Update (toggle t t',sz) -- TODO

split : Zipper -> Result
split (t, z) = case Core.Array.doPropagatingSplits Span.toValue Span.startZipper Span.endZipper (Span.do Core.String.split) z of
  Span.Update z -> Update (t,z)
  Span.Split lefts z rights -> Split (map (\s -> (t,s)) lefts) (t,z) (map (\s -> (t,s)) rights)
  Span.Delete -> Delete
  Span.EnterNext -> EnterNext
  Span.EnterPrev -> EnterPrev
  Span.NoChange -> NoChange

backspace : Zipper -> Result
backspace (t,z) = case Core.Array.doPropagatingSplits Span.toValue Span.startZipper Span.endZipper (Span.do Core.String.backspace) z of
  Span.Update z' -> Update (t,z')
  Span.NoChange -> Join (t, z |> Core.Array.toValue Span.toValue)
  _ -> NoChange
