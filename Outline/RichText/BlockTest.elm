module Outline.RichText.BlockTest (suite) where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

import Outline.RichText.Block.Model (..)
import Outline.RichText.Block.Actions (..)
import RichText

span = RichText.span

actionsTest = Suite "Actions"
  [ Suite "toggleStyle"
    [ test "with no selection, applies to current block" <|
      toggleStyle Task (paragraph (span "ab") |> allZipper)
      `assertEqual`
      Update (value Task [(span "ab")] |> allZipper)
    , test "if style is already set, switches to paragraph" <|
      toggleStyle Task (value Task [(span "ab")] |> allZipper)
      `assertEqual`
      Update (value Paragraph [(span "ab")] |> allZipper)
    ]
  , Suite "split"
    [ test "creates new paragraph" <|
      split (paragraph (span "ab") |> startZipper)
      `assertEqual`
      Split [paragraph (span "")] (paragraph (span "ab") |> startZipper) []
    , test "in task block, creates new paragraph" <|
      split (value Task [(span "ab")] |> startZipper)
      `assertEqual`
      Split [value Task [(span "")]] (paragraph (span "ab") |> startZipper) []
    ]
  , Suite "backspace"
    [ test "tries to backspace text" <|
      backspace (paragraph (span "ab") |> endZipper)
      `assertEqual`
      Update (paragraph (span "a") |> endZipper)
    , test "joins if at start of block" <|
      backspace (paragraph (span "ab") |> startZipper)
      `assertEqual`
      Join (paragraph (span "ab"))
    ]
  ]

suite = Suite "Outline.RichText.Block"
  [ actionsTest
  ]
