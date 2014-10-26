module Test where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

import Test.Core.ArrayTest

import Outline.Entry as Entry
import Outline.Entry (BaseCursor(..), entry)
import Core.Action as Action

tests : Test
tests = Suite "backspace"
        [ Entry.backspace (entry "Elm" "" []) (InText 1)
            `equals` (entry "lm" "" [], InText 0)
        ]

enterTest = Suite "enter"
  [ Entry.enter (entry "" "" [entry "ab" "" []]) (InChild (0,InText 1))
      `equals` (entry "" "" [entry "a" "" [], entry "b" "" []], InChild (1, InText 0))
--  [ Action.apply Entry.enter tree (InChild 0 (InText 3))
--      `equals` (entry "Elm" "" [ entry "Sta" "" [], entry "dard Libraries"])
  ]

suite = Suite "Outline.Entry" [tests, enterTest, Test.Core.ArrayTest.suite]
