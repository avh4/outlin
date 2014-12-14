module Tests where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

import Test.Core.ArrayTest
import Test.Outline.EntryTest
import Test.MainTest

all = Suite "outlin"
  [ Test.Core.ArrayTest.suite
  , Test.Outline.EntryTest.suite
  , Test.MainTest.suite
  ]
