module AllTests where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

import Test.Core.ArrayTest
import Test.Core.StringTest
import Test.Outline.SpanTest
import Test.Outline.BlockTest
import Test.Outline.EntryTest
import Test.MainTest

all = Suite "outlin"
  [ Test.Core.StringTest.suite
  , Test.Core.ArrayTest.suite
  , Test.Outline.SpanTest.suite
  , Test.Outline.BlockTest.suite
  , Test.Outline.EntryTest.suite
  , Test.MainTest.suite
  ]
