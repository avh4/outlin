module Test.Core.ArrayTest where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

import Core.Action as Action
import Core.Array as Array
import Core.String

import String

split = Array.split Core.String.toValue Core.String.startZipper Core.String.endZipper Core.String.split_
do = Array.do Core.String.toValue Core.String.startZipper Core.String.endZipper

splitTest = Suite "split"
  [ split ([],("ab",1),[])
      `equals` Action.Update (["a"],("b",0),[])
  , split (["a"],("xy",1),["b"])
      `equals` Action.Update (["x","a"],("y",0),["b"])
  , do (Action.always Action.Delete) ([],("a",0),["b"])
      `equals` Action.Update ([],("b",0),[])
  , test "can delete the last item" <|
    do (Action.always Action.Delete) ([],("a",0),[])
      `assertEqual` Action.Delete
  , test "can delete the terminal item" <|
    do (Action.always Action.Delete) (["a"], ("b",0), [])
      `assertEqual` Action.Update ([], ("a",1), [])
  ]

doTest = Suite "do" <|
  let goNext = \z -> do (Action.always Action.EnterNext) z
      goPrev = \z -> do (Action.always Action.EnterPrev) z
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
    goPrev (["aaa"],("b",0),[])
      `assertEqual` Action.Update ([],("aaa",3),["b"])
  , test "EnterPrev from last item goes out" <|
    goPrev ([],("a",0),["b"])
      `assertEqual` Action.EnterPrev
  , test "EnterNext uses prevCursor" <|
    goPrev (["aaa"],("b",7),[])
      `assertEqual` Action.Update ([],("aaa",3),["b"])
  ]

suite = Suite "Core.Array"
  [ doTest
  , splitTest
  ]
