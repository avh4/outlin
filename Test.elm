module Test where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

import Test.Core.ArrayTest
import Test.Outline.EntryTest

suite = Suite "outlin"
  [ Test.Core.ArrayTest.suite
  , Test.Outline.EntryTest.suite
  ]
