module Test.Outline.RichTextTest where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

import Core.Action as Action
import Outline.RichText.Model (..)
import Outline.RichText.Span.Model (..)
import Outline.RichText.Span.Model as Span
import Core.Array

splitTest = Suite "Model.split"
  [ test "with no split" <|
    split ";" (Core.Array.value [Span.value Bold "ab"])
    `assertEqual`
    [ Core.Array.value [Span.value Bold "ab"] ]
  , test "splitting a single span" <|
    split ";" (Core.Array.value [Span.value Bold "a;b"])
    `assertEqual`
    [ Core.Array.value [Span.value Bold "a"]
    , Core.Array.value [Span.value Bold "b"]
    ]
  , test "split in the first span" <|
    split ";" (Core.Array.value [Span.value Bold "a;b", Span.value Normal "c"])
    `assertEqual`
    [ Core.Array.value [Span.value Bold "a"]
    , Core.Array.value [Span.value Bold "b", Span.value Normal "c"]
    ]
  , test "split in the second span" <|
    split ";" (Core.Array.value [Span.value Bold "a", Span.value Normal "b;c"])
    `assertEqual`
    [ Core.Array.value [Span.value Bold "a", Span.value Normal "b"]
    , Core.Array.value [Span.value Normal "c"]
    ]
  , test "multiple splits in a span" <|
    split ";" (Core.Array.value [Span.value Bold "a;b;c;d"])
    `assertEqual`
    [ Core.Array.value [Span.value Bold "a"]
    , Core.Array.value [Span.value Bold "b"]
    , Core.Array.value [Span.value Bold "c"]
    , Core.Array.value [Span.value Bold "d"]
    ]
  ]

suite = Suite "Outline.RichText"
  [ splitTest
  ]
