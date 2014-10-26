module Test.Core.ArrayTest where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

import Core.Action as Action
import Core.Array as Array

import String

stringSplit = (\s n -> (String.left n s, String.dropLeft n s, 0))

splitTest = Suite "split"
  [ Array.split stringSplit ["ab"] (0,1)
      `equals` Action.Update ["a", "b"] (1,0)
  , Array.split stringSplit ["a", "xy", "b"] (1,1)
      `equals` Action.Update ["a", "x", "y", "b"] (2,0)
  , Array.do (Action.always Action.Delete) ["a", "b"] (0,0)
      `equals` Action.Update ["b"] (0,0)
  , Array.do (Action.always Action.Delete) ["a"] (0,0)
      `equals` Action.Delete
  ]

suite = Suite "Core.Array" [splitTest]
