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

navTest = Suite "navigation"
  [ test "can go left in text" <|
    Entry.goLeft (textEntry "ab") (InText 1)
      `assertEqual` Action.Update (textEntry "ab") (InText 0)
  , test "go left stops at the edge" <|
    Entry.goLeft (textEntry "ab") (InText 0)
      `assertEqual` Action.Update (textEntry "ab") (InText 0)
  , test "can go right in text" <|
    Entry.goRight (textEntry "ab") (InText 1)
      `assertEqual` Action.Update (textEntry "ab") (InText 2)
  , test "go right stops at the edge" <|
    Entry.goRight (textEntry "ab") (InText 2)
      `assertEqual` Action.Update (textEntry "ab") (InText 2)
  ]

editTest = Suite "basic editing"
  [ test "can insert text" <|
    Entry.insert "xx" (textEntry "ab") (InText 1)
      `assertEqual` Action.Update (textEntry "axxb") (InText 3)
  , test "can backspace text" <|
    Entry.backspace (textEntry "Elm") (InText 1)
      `assertEqual` Action.Update (textEntry "lm") (InText 0)
  , test "backspace stops at edge" <|
    Entry.backspace (textEntry "Elm") (InText 0)
      `assertEqual` Action.NoChange
  ]

enterTest = Suite "enter"
  [ test "can split an Entry" <|
    Entry.enter (textEntry "ab") (InText 1)
      `assertEqual` Action.Split [textEntry "a", textEntry "b"] 1 (InText 0)
  , test "can split a child Entry" <|
    Entry.enter (entry "" "" [] [textEntry "ab"]) (InChild (0,InText 1))
      `assertEqual` Action.Update (entry "" "" [] [textEntry "a", textEntry "b"]) (InChild (1, InText 0))
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
  , test "can delete the last child" <|
    Entry.delete (entry "" "" [] [textEntry "a"]) (InChild (0,InText 0))
      `assertEqual` Action.Update (entry "" "" [] []) (InText 0)
  ]

suite = Suite "Outline.Entry" [navTest, editTest, backspaceTest, enterTest, deleteTest]
