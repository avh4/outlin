module RichText.SpanZipper
  ( SpanZipper
  , toValue, zipper
  ) where

import Core.String as String
import RichText (..)

type alias SpanZipper = (SpanType, String.Zipper)

toValue : SpanZipper -> Span
toValue (t,z) = (t, String.toValue z)

zipper : (String -> String.Zipper) -> Span -> SpanZipper
zipper fn (t,s) = (t, fn s)
