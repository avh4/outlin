module Test.Outline.BlockTest (suite) where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

import Core.Action (..)
import Outline.RichText.Block.Model (..)
import Outline.RichText.Block.Actions (..)
import Outline.RichText.Span.Model as Span

actionsTest = Suite "Actions"
  [ Suite "toggleStyle"
    [ test "with no selection, applies to current block" <|
      toggleStyle Task (paragraph (Span.normal "ab") |> allZipper)
      `assertEqual`
      Update (value Task [(Span.normal "ab")] |> allZipper)
    , test "if style is already set, switches to paragraph" <|
      toggleStyle Task (value Task [(Span.normal "ab")] |> allZipper)
      `assertEqual`
      Update (value Paragraph [(Span.normal "ab")] |> allZipper)
    ]
  , Suite "split"
    [ test "creates new paragraph" <|
      split (paragraph (Span.normal "ab") |> startZipper)
      `assertEqual`
      Split [paragraph (Span.normal "")] (paragraph (Span.normal "ab") |> startZipper) []
    ]
  ]

suite = Suite "Outline.RichText.Block"
  [ actionsTest
  ]
