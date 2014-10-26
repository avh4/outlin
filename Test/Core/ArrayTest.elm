module Test.Core.ArrayTest where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

import Core.Action as Action
import Core.Array as Array

import String

stringSplit = (\s n -> (String.left n s, String.dropLeft n s, 0))

splitTest = Suite "split"
  [ Array.split 0 stringSplit ["ab"] (0,1)
      `equals` Action.Update ["a", "b"] (1,0)
  , Array.split 0 stringSplit ["a", "xy", "b"] (1,1)
      `equals` Action.Update ["a", "x", "y", "b"] (2,0)
  , Array.do 0 (Action.always Action.Delete) ["a", "b"] (0,0)
      `equals` Action.Update ["b"] (0,0)
  , test "can delete the last item" <|
    Array.do 0 (Action.always Action.Delete) ["a"] (0,0)
      `assertEqual` Action.Delete
  , test "can delete the terminal item" <|
    Array.do 0 (Action.always Action.Delete) ["a", "b"] (1,0)
      `assertEqual` Action.Update ["a"] (0,0)
  ]

doTest = Suite "do" <|
  let goNext = Array.do 0 (Action.always Action.EnterNext)
      goPrev = Array.do 0 (Action.always Action.EnterPrev)
  in
  [ test "EnterNext goes to next item" <|
    goNext ["a","b"] (0,0)
      `assertEqual` Action.Update ["a","b"] (1,0)
  , test "EnterNext from last item goes out" <|
    goNext ["a","b"] (1,0)
      `assertEqual` Action.EnterNext
  , test "EnterNext uses nextCursor" <|
    goNext ["a","b"] (0,7)
      `assertEqual` Action.Update ["a","b"] (1,0)
  , test "EnterPrev goes to prev item" <|
    goPrev ["a","b"] (1,0)
      `assertEqual` Action.Update ["a","b"] (0,0)
  , test "EnterPrev from last item goes out" <|
    goPrev ["a","b"] (0,0)
      `assertEqual` Action.EnterPrev
  ]

suite = Suite "Core.Array" [doTest, splitTest]
