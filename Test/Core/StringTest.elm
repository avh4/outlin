module Test.Core.StringTest (suite) where

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

suite = Suite "Core.String"
  [ zippersTest
  ]
