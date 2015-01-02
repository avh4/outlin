module Core.StringTest (suite) where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

import Core.Action (..)
import Core.String (..)

zippersTest = Suite "(zippers)"
  [ Suite "rangeZipper"
    [ test "inner range" <|
      rangeZipper (1,1) "abc"
      `assertEqual`
      ("a", "b", "c")
    ]
  ]

actionsTest = Suite "(actions)"
  [ Suite "selectToStartOfLine"
    [ test "with no newlines, selects to beginning" <|
      selectToStartOfLine ("a", "b", "c")
      `assertEqual`
      Update ("", "ab", "c")
    , test "selects to start of line" <|
      selectToStartOfLine ("x\na", "b", "c")
      `assertEqual`
      Update ("x\n", "ab", "c")
    ]
  , Suite "moveToEndOfLine"
    [ test "in last line" <|
      moveToEndOfLine ("a", "b", "c")
      `assertEqual`
      Update ("abc", "", "")
    , test "in early line" <|
      moveToEndOfLine ("a", "b", "c\nx")
      `assertEqual`
      Update ("abc", "", "\nx")
    ]
  , Suite "moveToStartOfLine"
    [ test "in first line" <|
      moveToStartOfLine ("a", "b", "c")
      `assertEqual`
      Update ("", "", "abc")
    , test "in late line" <|
      moveToStartOfLine ("x\na", "b", "c")
      `assertEqual`
      Update ("x\n", "", "abc")
    ]
  ]

suite = Suite "Core.String"
  [ zippersTest
  , actionsTest
  ]
