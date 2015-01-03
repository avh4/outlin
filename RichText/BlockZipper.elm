module RichText.BlockZipper
  ( BlockZipper
  , toValue
  , mergeZipper, startZipper, endZipper, allZipper
  ) where

import RichText (..)
import RichText.SpanZipper as Span
import RichText.SpanZipper (SpanZipper)
import Core.Array
import Core.String

type alias BlockZipper = (BlockType, Core.Array.Zipper Span SpanZipper)

toValue : BlockZipper -> Block
toValue (t,s) = (t, Core.Array.toValue Span.toValue s)

mergeZipper : Block -> Block -> BlockZipper
mergeZipper (t1,spans1) (t2,spans2) = case spans2 of
  [] -> (t1,spans1) |> endZipper
  (head::rest) -> (t1, Core.Array.zipper spans1 (head |> Span.zipper Core.String.startZipper) rest)

startZipper : Block -> BlockZipper
startZipper (t,s) = s
  |> Core.Array.firstZipperOr (span "") (Span.zipper Core.String.startZipper)
  |> (\s' -> (t,s'))

endZipper : Block -> BlockZipper
endZipper (t,s) = s
  |> Core.Array.lastZipperOr (span "") (Span.zipper Core.String.endZipper)
  |> (\s' -> (t,s'))

allZipper : Block -> BlockZipper
allZipper (t,s) = s
  |> Core.Array.lastZipperOr (span "") (Span.zipper Core.String.allZipper)
  |> (\s' -> (t,s'))
