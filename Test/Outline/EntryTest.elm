module Test.Outline.EntryTest where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

import Core.Action as Action
import Outline.Entry as Entry
import Outline.Entry (entry, BaseCursor(..))

textEntry s = entry s "" [] []

simpleTree = (entry "root" "" [] [textEntry "a", textEntry "b"])

backspaceTest : Test
backspaceTest = Suite "backspace"
  [ Entry.backspace (entry "Elm" "" [] []) (InText 1)
      `equals` Action.Update (entry "lm" "" [] []) (InText 0)
  ]

navTest = Suite "navigation"
  [ Suite "text"
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
  , Suite "children"
    [ test "can go to next child" <|
      Entry.goNext simpleTree (InChild (0,InText 0))
        `assertEqual` Action.Update simpleTree (InChild (1,InText 0))
    , test "can go to prev child" <|
      Entry.goPrev simpleTree (InChild (1,InText 0))
        `assertEqual` Action.Update simpleTree (InChild (0,InText 0))
    , test "can go into child" <|
      Entry.goNext simpleTree (InText 0)
        `assertEqual` Action.Update simpleTree (InChild (0,InText 0))
    , test "can go out of child" <|
      Entry.goPrev simpleTree (InChild (0,InText 0))
        `assertEqual` Action.Update simpleTree (InText 0)
    , test "can go to next parent" <|
      let tree = (entry "" "" [] [entry "" "" [] [textEntry "a"], entry "" "" [] []])
      in Entry.goNext tree (InChild (0,InChild (0,InText 0)))
        `assertEqual` Action.Update tree (InChild (1,InText 0))
    , test "can go to child of previous parent" <|
      let tree = (entry "" "" [] [entry "" "" [] [textEntry "a"], entry "" "" [] []])
      in Entry.goPrev tree (InChild (1,InText 0))
        `assertEqual` Action.Update tree (InChild (0,InChild (0,InText 0)))
    ]
  , Suite "inbox" <|
    let tree = (entry "" "" ["a","b"] [])
    in
    [ test "can go to next inbox item" <|
      Entry.goNext tree (InInbox (0,0))
        `assertEqual` Action.Update tree (InInbox (1,0))
    , test "can enter inbox item" <|
      Entry.goNext tree (InText 0)
        `assertEqual` Action.Update tree (InInbox (0,0))
    , test "can exit last inbox item into children" <|
      Entry.goNext (entry "" "" ["a","b"] [textEntry "x"]) (InInbox (1,0))
        `assertEqual` Action.Update (entry "" "" ["a","b"] [textEntry "x"]) (InChild (0,InText 0))
    , test "can exit last inbox item with no children" <|
      Entry.goNext tree (InInbox (1,0))
        `assertEqual` Action.EnterNext
    , test "can go to prev inbox item" <|
      Entry.goPrev tree (InInbox (1,0))
        `assertEqual` Action.Update tree (InInbox (0,0))
    , test "can enter inbox from bottom" <|
      Entry.goPrev (entry "" "" ["a","b"] [textEntry "x"]) (InChild (0,InText 0))
        `assertEqual` Action.Update (entry "" "" ["a","b"] [textEntry "x"]) (InInbox (1,0))
    , test "can exit first inbox item" <|
      Entry.goPrev tree (InInbox (0,0))
        `assertEqual` Action.Update tree (InText 0)
    -- , test "can exit last inbox item with no children" <|
    --   Entry.goPrev tree (InInbox (1,0))
    --     `assertEqual` Action.EnterNext
    ]
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

promoteTest = Suite "promote" <|
  [ test "moves current inbox item to children" <|
    Entry.promote (entry "" "" ["a","b"] []) (InInbox (0,0))
    `assertEqual` Action.Update (entry "" "" ["b"] [textEntry "a"]) (InInbox (0,0))
  , test "makes the new item the first child" <|
    Entry.promote (entry "" "" ["a","b"] [textEntry "x"]) (InInbox (0,0))
    `assertEqual` Action.Update (entry "" "" ["b"] [textEntry "a", textEntry "x"]) (InInbox (0,0))
  , test "with terminal inbox item, moves cursor up" <|
    Entry.promote (entry "" "" ["a","b"] [textEntry "x"]) (InInbox (1,0))
    `assertEqual` Action.Update (entry "" "" ["a"] [textEntry "b", textEntry "x"]) (InInbox (0,0))
  , test "with the last inbox item, cursor follows to children" <|
    Entry.promote (entry "" "" ["a"] [textEntry "x"]) (InInbox (0,0))
    `assertEqual` Action.Update (entry "" "" [] [textEntry "a", textEntry "x"]) (InChild (0,InText 0))
  ]

suite = Suite "Outline.Entry"
  [ navTest
  , editTest
  , backspaceTest
  , enterTest
  , deleteTest
  , promoteTest
  ]
