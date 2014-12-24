module Test.Outline.SpanTest (suite) where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

import Core.Action (..)
import Outline.RichText.Span.Model (..)
import Outline.RichText.Span.Actions (..)

actionsTest = Suite "Actions"
  [ Suite "applyStyle"
    [ test "with all text selected, changes style" <|
      applyStyle Bold (value Normal "ab" |> allZipper)
      `assertEqual`
      Update (value Bold "ab" |> allZipper)
    , test "with start selected, splits new span" <|
      applyStyle Bold (value Normal "abcd" |> rangeZipper (0,2))
      `assertEqual`
      Split [] (value Bold "ab" |> allZipper) [value Normal "cd"]
    , test "with end selected, splits new span" <|
      applyStyle Bold (value Normal "abcd" |> rangeZipper (2,2))
      `assertEqual`
      Split [value Normal "ab"] (value Bold "cd" |> allZipper) []
    , test "with middle selected, splits new span" <|
      applyStyle Bold (value Normal "abcd" |> rangeZipper (1,2))
      `assertEqual`
      Split [value Normal "a"] (value Bold "bc" |> allZipper) [value Normal "d"]
    , test "with no selection, no change" <|
      applyStyle Bold (value Normal "abcd" |> startZipper)
      `assertEqual`
      NoChange
    ]
  ]

suite = Suite "Outline.RichText.Span"
  [ actionsTest
  ]
