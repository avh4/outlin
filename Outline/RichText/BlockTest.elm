module Outline.RichText.BlockTest (suite) where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

import Outline.RichText.Block.Actions (..)
import RichText (..)
import RichText.BlockZipper (..)

value t s = (t,s)

actionsTest = Suite "Actions"
  [ Suite "toggleStyle"
    [ test "with no selection, applies to current block" <|
      toggleStyle Task (paragraph "ab" |> allZipper)
      `assertEqual`
      (value Task [(span "ab")] |> allZipper)
    , test "if style is already set, switches to paragraph" <|
      toggleStyle Task (value Task [(span "ab")] |> allZipper)
      `assertEqual`
      (value Paragraph [(span "ab")] |> allZipper)
    ]
  , Suite "split"
    [ test "creates new paragraph" <|
      split (paragraph "ab" |> startZipper)
      `assertEqual`
      Split [paragraph ""] (paragraph "ab" |> startZipper) []
    , test "in task block, creates new paragraph" <|
      split (value Task [(span "ab")] |> startZipper)
      `assertEqual`
      Split [value Task [(span "")]] (paragraph "ab" |> startZipper) []
    ]
  , Suite "backspace"
    [ test "tries to backspace text" <|
      backspace (paragraph "ab" |> endZipper)
      `assertEqual`
      Update (paragraph "a" |> endZipper)
    , test "joins if at start of block" <|
      backspace (paragraph "ab" |> startZipper)
      `assertEqual`
      Join (paragraph "ab")
    ]
  ]

suite = Suite "Outline.RichText.Block"
  [ actionsTest
  ]
