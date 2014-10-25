module Test.Core.ArrayTest where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

import Core.Action as Action
import Core.Array as Array

import String

splitTest = Suite "split"
  [ Action.apply (Array.split (\s n -> (String.left n s, String.dropLeft n s, 0))) ["ab"] (0, 1)
      `equals` (["a", "b"], (1, 0))
  ]

suite = Suite "Core.Array" [splitTest]
