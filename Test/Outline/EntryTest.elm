module Test.Outline.EntryTest where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

import Core.Action as Action
import Outline.Entry as Entry
import Outline.Entry (entry, BaseCursor(..))

textEntry s = entry s "" [] []

backspaceTest : Test
backspaceTest = Suite "backspace"
  [ Entry.backspace (entry "Elm" "" [] []) (InText 1)
      `equals` Action.Update (entry "lm" "" [] []) (InText 0)
  ]

enterTest = Suite "enter"
  [ Entry.enter (entry "" "" [] [textEntry "ab"]) (InChild (0,InText 1))
      `equals` Action.Update (entry "" "" [] [textEntry "a", textEntry "b"]) (InChild (1, InText 0))
--  [ Action.apply Entry.enter tree (InChild 0 (InText 3))
--      `equals` (entry "Elm" "" [ entry "Sta" "" [], entry "dard Libraries"])
  ]

deleteTest = Suite "delete"
  [ test "can delete an inbox item" <|
    Entry.delete (entry "" "" ["a", "b"] []) (InInbox (0,0))
      `assertEqual` Action.Update (entry "" "" ["b"] []) (InInbox (0,0))
  , test "can delete the last inbox item" <|
    Entry.delete (entry "" "" ["a"] []) (InInbox (0,0))
      `assertEqual` Action.Update (entry "" "" [] []) (InText 0)
  , test "can delete a child" <|
    Entry.delete (entry "" "" [] [textEntry "a", textEntry "b"]) (InChild (0,InText 0))
      `assertEqual` Action.Update (entry "" "" [] [textEntry "b"]) (InChild (0,InText 0))
  , test "can delete the terminal child" <|
    Entry.delete (entry "" "" [] [textEntry "a", textEntry "b"]) (InChild (1,InText 0))
      `assertEqual` Action.Update (entry "" "" [] [textEntry "a"]) (InChild (0,InText 0))
  ]

suite = Suite "Outline.Entry" [backspaceTest, enterTest, deleteTest]
