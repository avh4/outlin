module Test.Outline.EntryTest where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

import Core.Action as Action
import Outline.Entry as Entry
import Outline.Entry (entry, BaseCursor(..))

backspaceTest : Test
backspaceTest = Suite "backspace"
  [ Entry.backspace (entry "Elm" "" [] []) (InText 1)
      `equals` Action.Update (entry "lm" "" [] []) (InText 0)
  ]

enterTest = Suite "enter"
  [ Entry.enter (entry "" "" [] [entry "ab" "" [] []]) (InChild (0,InText 1))
      `equals` Action.Update (entry "" "" [] [entry "a" "" [] [], entry "b" "" [] []]) (InChild (1, InText 0))
--  [ Action.apply Entry.enter tree (InChild 0 (InText 3))
--      `equals` (entry "Elm" "" [ entry "Sta" "" [], entry "dard Libraries"])
  ]

deleteTest = Suite "delete"
  [ Entry.delete (entry "" "" ["a", "b"] []) (InInbox (0,0))
      `equals` Action.Update (entry "" "" ["b"] []) (InInbox (0,0))
  ]

suite = Suite "Outline.Entry" [backspaceTest, enterTest, deleteTest]
