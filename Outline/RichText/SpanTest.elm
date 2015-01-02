module Outline.RichText.SpanTest (suite) where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

import Core.Action (..)
import Outline.RichText.Span.Actions (..)
import RichText (..)
import Core.String

startZipper = zipper Core.String.startZipper
allZipper = zipper Core.String.allZipper
rangeZipper r = zipper (Core.String.rangeZipper r)

actionsTest = Suite "Actions"
  [ Suite "applyStyle"
    [ test "with all text selected, changes style" <|
      applyStyle Bold (span "ab" |> allZipper)
      `assertEqual`
      Update (bold "ab" |> allZipper)
    , test "with start selected, splits new span" <|
      applyStyle Bold (span "abcd" |> rangeZipper (0,2))
      `assertEqual`
      Split [] (bold "ab" |> allZipper) [span "cd"]
    , test "with end selected, splits new span" <|
      applyStyle Bold (span "abcd" |> rangeZipper (2,2))
      `assertEqual`
      Split [span "ab"] (bold "cd" |> allZipper) []
    , test "with middle selected, splits new span" <|
      applyStyle Bold (span "abcd" |> rangeZipper (1,2))
      `assertEqual`
      Split [span "a"] (bold "bc" |> allZipper) [span "d"]
    , test "with no selection, no change" <|
      applyStyle Bold (span "abcd" |> startZipper)
      `assertEqual`
      NoChange
    ]
  ]

suite = Suite "Outline.RichText.Span"
  [ actionsTest
  ]
