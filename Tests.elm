module Tests where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

import Test.Core.ArrayTest
import Test.Core.StringTest
import Test.Outline.EntryTest
import Test.Outline.RichTextTest
import Test.Outline.SpanTest
import Test.MainTest

all = Suite "outlin"
  [ Test.Core.StringTest.suite
  , Test.Core.ArrayTest.suite
  , Test.Outline.EntryTest.suite
  , Test.Outline.RichTextTest.suite
  , Test.Outline.SpanTest.suite
  , Test.MainTest.suite
  ]
