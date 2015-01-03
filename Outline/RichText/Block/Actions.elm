module Outline.RichText.Block.Actions
  ( Result(..)
  , doSpan
  , toggleStyle, split, backspace
  ) where

import Core.Array
import Core.String
import Core.Action as Span
import Outline.RichText.Span.Actions as Span
import List (map)
import RichText (..)
import RichText.SpanZipper as RichText
import RichText.BlockZipper (..)

type Result
  = Update BlockZipper
  | Split (List Block) BlockZipper (List Block)
  | Join Block
  | Delete
  | EnterPrev | EnterNext
  | NoChange

doSpan : (RichText.SpanZipper -> Span.Result) -> BlockZipper -> Result
doSpan spanFn (t,z) = case Core.Array.doPropagatingSplits RichText.toValue (RichText.zipper Core.String.startZipper) (RichText.zipper Core.String.endZipper) spanFn z of
  Span.Update z -> Update (t,z)
  Span.Split lefts z rights -> Split (map (\s -> (t,s)) lefts) (Paragraph,z) (map (\s -> (Paragraph,s)) rights)
  Span.Delete -> Delete
  Span.EnterNext -> EnterNext
  Span.EnterPrev -> EnterPrev
  Span.NoChange -> NoChange

toggle : BlockType -> BlockType -> BlockType
toggle current t = if current == t then Paragraph else t

toggleStyle : BlockType -> BlockZipper -> Result
toggleStyle t' (t,sz) = Update (toggle t t',sz) -- TODO

split : BlockZipper -> Result
split (t, z) = case Core.Array.doPropagatingSplits RichText.toValue (RichText.zipper Core.String.startZipper) (RichText.zipper Core.String.endZipper) (Span.do Core.String.split) z of
  Span.Update z -> Update (t,z)
  Span.Split lefts z rights -> Split (map (\s -> (t,s)) lefts) (Paragraph,z) (map (\s -> (Paragraph,s)) rights)
  Span.Delete -> Delete
  Span.EnterNext -> EnterNext
  Span.EnterPrev -> EnterPrev
  Span.NoChange -> NoChange

backspace : BlockZipper -> Result
backspace (t,z) = case Core.Array.doPropagatingSplits RichText.toValue (RichText.zipper Core.String.startZipper) (RichText.zipper Core.String.endZipper) (Span.do Core.String.backspace) z of
  Span.Update z' -> Update (t,z')
  Span.NoChange -> Join (t, z |> Core.Array.toValue RichText.toValue)
  _ -> NoChange
