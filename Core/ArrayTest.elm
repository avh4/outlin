module Core.ArrayTest where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

import Core.Action as Action
import Core.Array as Array
import Core.String

import String

do = Array.do Core.String.toValue Core.String.startZipper Core.String.endZipper
split = do Core.String.split
sz = Core.String.zipper

splitTest = Suite "split"
  [ split ([],sz "a" "b",[])
      `equals` Action.Update (["a"],sz "" "b",[])
  , split (["a"],sz "x" "y",["b"])
      `equals` Action.Update (["x","a"],sz "" "y",["b"])
  , do (Action.always Action.Delete) ([],sz "" "a",["b"])
      `equals` Action.Update ([],sz "" "b",[])
  , test "can delete the last item" <|
    do (Action.always Action.Delete) ([],sz "" "a",[])
      `assertEqual` Action.Delete
  , test "can delete the terminal item" <|
    do (Action.always Action.Delete) (["a"], sz "" "b", [])
      `assertEqual` Action.Update ([],sz "a" "", [])
  ]

doTest = Suite "do" <|
  let goNext = \z -> do (Action.always Action.EnterNext) z
      goPrev = \z -> do (Action.always Action.EnterPrev) z
  in
  [ test "EnterNext goes to next item" <|
    goNext ([],sz "" "a",["b"])
      `assertEqual` Action.Update (["a"],sz "" "b",[])
  , test "EnterNext from last item goes out" <|
    goNext (["a"],sz "" "b",[])
      `assertEqual` Action.EnterNext
  , test "EnterNext uses nextCursor" <|
    goNext ([],sz "a" "",["b"])
      `assertEqual` Action.Update (["a"],sz "" "b",[])
  , test "EnterPrev goes to prev item" <|
    goPrev (["aaa"],sz "" "b",[])
      `assertEqual` Action.Update ([],sz "aaa" "",["b"])
  , test "EnterPrev from last item goes out" <|
    goPrev ([],sz "" "a",["b"])
      `assertEqual` Action.EnterPrev
  , test "EnterNext uses prevCursor" <|
    goPrev (["aaa"],sz "b" "",[])
      `assertEqual` Action.Update ([],sz "aaa" "",["b"])
  ]

suite = Suite "Core.Array"
  [ doTest
  , splitTest
  ]
