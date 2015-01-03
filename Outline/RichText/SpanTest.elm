module Outline.RichText.SpanTest (suite) where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

import Core.Action (..)
import Outline.RichText.Span.Actions (..)
import RichText (..)
import RichText.SpanZipper (..)
import Core.String

startZipper = zipper Core.String.startZipper
allZipper = zipper Core.String.allZipper
rangeZipper r = zipper (Core.String.rangeZipper r)

actionsTest = Suite "Actions"
  [ Suite "styleSpanSelection"
    [ test "with all text selected, changes style" <|
      styleSpanSelection Bold (span "ab" |> allZipper)
      `assertEqual`
      (Nothing, bold "ab" |> allZipper, Nothing)
    , test "with start selected, splits new span" <|
      styleSpanSelection Bold (span "abcd" |> rangeZipper (0,2))
      `assertEqual`
      (Nothing, bold "ab" |> allZipper, Just <| span "cd")
    , test "with end selected, splits new span" <|
      styleSpanSelection Bold (span "abcd" |> rangeZipper (2,2))
      `assertEqual`
      (Just <| span "ab", bold "cd" |> allZipper, Nothing)
    , test "with middle selected, splits new span" <|
      styleSpanSelection Bold (span "abcd" |> rangeZipper (1,2))
      `assertEqual`
      (Just <| span "a", bold "bc" |> allZipper, Just <| span "d")
    , test "with no selection, no change" <|
      styleSpanSelection Bold (span "abcd" |> startZipper)
      `assertEqual`
      (Nothing, bold "" |> startZipper, Just <| span "abcd")
    ]
  ]

suite = Suite "Outline.RichText.Span"
  [ actionsTest
  ]
