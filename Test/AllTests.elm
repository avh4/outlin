module Test.AllTests where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

import Core.ArrayTest
import Core.StringTest
import Outline.RichText.SpanTest
import Outline.RichText.BlockTest
import Outline.EntryTest
import Test.IntegrationTest
import Test.Migration.ScratchTest

all = Suite "outlin"
  [ Core.StringTest.suite
  , Core.ArrayTest.suite
  , Outline.RichText.SpanTest.suite
  , Outline.RichText.BlockTest.suite
  , Outline.EntryTest.suite
  , Test.IntegrationTest.suite
  , Test.Migration.ScratchTest.suite
  ]
