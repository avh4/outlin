module Test.Core.ArrayTest where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

import Core.Action as Action
import Core.Array as Array

import String

stringSplit = \s n -> (String.left n s, String.dropLeft n s, 0)

splitTest = Suite "split"
  [ Array.split 0 (\_ -> 0) stringSplit ([],("ab",1),[])
      `equals` Action.Update (["a"],("b",0),[])
  , Array.split 0 (\_ -> 0) stringSplit (["a"],("xy",1),["b"])
      `equals` Action.Update (["x","a"],("y",0),["b"])
  , Array.do 0 (\_ -> 0) (Action.always Action.Delete) ([],("a",0),["b"])
      `equals` Action.Update ([],("b",0),[])
  , test "can delete the last item" <|
    Array.do 0 (\_ -> 0) (Action.always Action.Delete) ([],("a",0),[])
      `assertEqual` Action.Delete
  , test "can delete the terminal item" <|
    Array.do 0 (\_ -> 0) (Action.always Action.Delete) (["a"], ("b",0), [])
      `assertEqual` Action.Update ([], ("a",0), [])
  ]

doTest = Suite "do" <|
  let goNext = \z -> Array.do 0 (\_ -> 9) (Action.always Action.EnterNext) z
      goPrev = \z -> Array.do 0 (\_ -> 9) (Action.always Action.EnterPrev) z
  in
  [ test "EnterNext goes to next item" <|
    goNext ([],("a",0),["b"])
      `assertEqual` Action.Update (["a"],("b",0),[])
  , test "EnterNext from last item goes out" <|
    goNext (["a"],("b",0),[])
      `assertEqual` Action.EnterNext
  , test "EnterNext uses nextCursor" <|
    goNext ([],("a",7),["b"])
      `assertEqual` Action.Update (["a"],("b",0),[])
  , test "EnterPrev goes to prev item" <|
    goPrev (["a"],("b",0),[])
      `assertEqual` Action.Update ([],("a",9),["b"])
  , test "EnterPrev from last item goes out" <|
    goPrev ([],("a",0),["b"])
      `assertEqual` Action.EnterPrev
  , test "EnterNext uses prevCursor" <|
    goPrev (["a"],("b",7),[])
      `assertEqual` Action.Update ([],("a",9),["b"])
  ]

suite = Suite "Core.Array" [doTest, splitTest]
