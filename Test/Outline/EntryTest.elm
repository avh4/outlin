module Test.Outline.EntryTest where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

import Core.Action as Action
import Outline.Entry as Entry
import Outline.Entry (entry,BaseZipper(..))
import Core.Array

textEntry s = entry s "" [] []

simpleTree = (entry "root" "" [] [textEntry "a", textEntry "b"])

backspaceTest : Test
backspaceTest = Suite "backspace"
  [ Entry.backspace (textEntry "Elm" |> Entry.textZipperAt 1)
      `equals` Action.Update (textEntry "lm" |> Entry.textZipperAt 0)
  ]

navTest = Suite "navigation"
  [ Suite "text"
    [ test "can go left in text" <|
      Entry.goLeft (textEntry "ab" |> Entry.textZipperAt 1)
        `assertEqual` Action.Update (textEntry "ab" |> Entry.textZipperAt 0)
    , test "go left stops at the edge" <|
      Entry.goLeft ((textEntry "ab") |> Entry.textZipperAt 0)
        `assertEqual` Action.Update ((textEntry "ab") |> Entry.textZipperAt 0)
    , test "can go right in text" <|
      Entry.goRight ((textEntry "ab") |> Entry.textZipperAt 1)
        `assertEqual` Action.Update ((textEntry "ab") |> Entry.textZipperAt 2)
    , test "go right stops at the edge" <|
      Entry.goRight ((textEntry "ab") |> Entry.textZipperAt 2)
        `assertEqual` Action.Update ((textEntry "ab") |> Entry.textZipperAt 2)
    ]
  , Suite "children"
    [ test "can go to next child" <|
      Entry.goNext (Entry.childZipper (Core.Array.firstZipper Entry.textZipper) simpleTree)
        `assertEqual` Action.Update (Entry.childZipper (Core.Array.lastZipper Entry.textZipper) simpleTree)
    , test "can go to prev child" <|
      Entry.goPrev (simpleTree |> (Entry.childZipper <| Core.Array.lastZipper Entry.textZipper))
        `assertEqual` Action.Update (simpleTree |> (Entry.childZipper <| Core.Array.firstZipper Entry.textZipper))
    , test "can go into child" <|
      Entry.goNext (simpleTree |> Entry.textZipper)
        `assertEqual` Action.Update (simpleTree |> (Entry.childZipper <| Core.Array.firstZipper Entry.textZipper))
    , test "can go out of child" <|
      Entry.goPrev (simpleTree |> (Entry.childZipper <| Core.Array.firstZipper Entry.textZipper))
        `assertEqual` Action.Update (simpleTree |> Entry.textZipper)
    , test "can go to next parent" <|
      let tree = (entry "" "" [] [entry "" "" [] [textEntry "a"], entry "" "" [] []])
      in Entry.goNext (tree |> (Entry.childZipper (Core.Array.firstZipper (Entry.childZipper <| Core.Array.firstZipper Entry.textZipper))))
        `assertEqual` Action.Update (tree |> (Entry.childZipper <| Core.Array.lastZipper Entry.textZipper))
    , test "can go to child of previous parent" <|
      let tree = (entry "" "" [] [entry "" "" [] [textEntry "a"], entry "" "" [] []])
      in Entry.goPrev (tree |> Entry.childZipper (Core.Array.lastZipper Entry.textZipper))
        `assertEqual` Action.Update (tree |> Entry.childZipper (Core.Array.firstZipper (Entry.childZipper (Core.Array.firstZipper Entry.textZipper))))
    ]
  , Suite "inbox" <|
    let tree = (entry "" "" [textEntry "a",textEntry "b"] [])
    in
    [ test "can go to next inbox item" <|
      Entry.goNext (tree |> Entry.inboxZipper (Core.Array.firstZipper Entry.textZipper))
        `assertEqual` Action.Update (tree |> Entry.inboxZipper (Core.Array.lastZipper Entry.textZipper))
    , test "can enter inbox item" <|
      Entry.goNext (tree |> Entry.textZipper)
        `assertEqual` Action.Update (tree |> Entry.inboxZipper (Core.Array.firstZipper Entry.textZipper))
    , test "can exit last inbox item into children" <|
      Entry.goNext ((entry "" "" [textEntry "a",textEntry "b"] [textEntry "x"]) |> Entry.inboxZipper (Core.Array.lastZipper Entry.textZipper))
        `assertEqual` Action.Update ((entry "" "" [textEntry "a",textEntry "b"] [textEntry "x"]) |> Entry.childZipper (Core.Array.firstZipper Entry.textZipper))
    , test "can exit last inbox item with no children" <|
      Entry.goNext (tree |> Entry.inboxZipper (Core.Array.lastZipper Entry.textZipper))
        `assertEqual` Action.EnterNext
    , test "can go to prev inbox item" <|
      Entry.goPrev (tree |> Entry.inboxZipper (Core.Array.lastZipper Entry.textZipper))
        `assertEqual` Action.Update (tree |> Entry.inboxZipper (Core.Array.firstZipper Entry.textZipper))
    , test "can enter inbox from bottom" <|
      Entry.goPrev ((entry "" "" [textEntry "a",textEntry "b"] [textEntry "x"]) |> Entry.childZipper (Core.Array.firstZipper Entry.textZipper))
        `assertEqual` Action.Update ((entry "" "" [textEntry "a",textEntry "b"] [textEntry "x"]) |> Entry.inboxZipper (Core.Array.lastZipper Entry.textZipper))
    , test "can exit first inbox item" <|
      Entry.goPrev (tree |> Entry.inboxZipper (Core.Array.firstZipper Entry.textZipper))
        `assertEqual` Action.Update (tree |> Entry.textZipper)
    ]
  ]

editTest = Suite "basic editing"
  [ test "can insert text" <|
    Entry.insert "xx" ((textEntry "ab") |> Entry.textZipperAt 1)
      `assertEqual` Action.Update ((textEntry "axxb") |> Entry.textZipperAt 3)
  , test "can backspace text" <|
    Entry.backspace ((textEntry "Elm") |> Entry.textZipperAt 1)
      `assertEqual` Action.Update ((textEntry "lm") |> Entry.textZipperAt 0)
  , test "backspace stops at edge" <|
    Entry.backspace ((textEntry "Elm") |> Entry.textZipperAt 0)
      `assertEqual` Action.NoChange
  ]

enterTest = Suite "enter"
  [ test "can split an Entry" <|
    Entry.enter ((textEntry "ab") |> Entry.textZipperAt 1)
      `assertEqual` Action.Split [textEntry "a"] (textEntry "b" |> Entry.textZipperAt 0) []
  , test "can split a child Entry" <|
    Entry.enter ((entry "" "" [] [textEntry "ab"]) |> Entry.childZipper (Core.Array.firstZipper <| Entry.textZipperAt 1))
      `assertEqual` Action.Update ((entry "" "" [] [textEntry "a", textEntry "b"]) |> Entry.childZipper (Core.Array.lastZipper <| Entry.textZipperAt 0))
  ]

deleteTest = Suite "delete"
  [ test "can delete an inbox item" <|
    Entry.delete ((entry "" "" [textEntry "a", textEntry "b"] []) |> Entry.inboxZipper (Core.Array.firstZipper Entry.textZipper))
      `assertEqual` Action.Update ((entry "" "" [textEntry "b"] []) |> Entry.inboxZipper (Core.Array.firstZipper Entry.textZipper))
  , test "can delete the last inbox item" <|
    Entry.delete ((entry "" "" [textEntry "a"] []) |> Entry.inboxZipper (Core.Array.firstZipper Entry.textZipper))
      `assertEqual` Action.Update ((entry "" "" [] []) |> Entry.textZipper)
  , test "can delete a child" <|
    Entry.delete ((entry "" "" [] [textEntry "a", textEntry "b"]) |> Entry.childZipper (Core.Array.firstZipper Entry.textZipper))
      `assertEqual` Action.Update ((entry "" "" [] [textEntry "b"]) |> Entry.childZipper (Core.Array.firstZipper Entry.textZipper))
  , test "can delete the terminal child" <|
    Entry.delete ((entry "" "" [] [textEntry "a", textEntry "b"]) |> Entry.childZipper (Core.Array.lastZipper Entry.textZipper))
      `assertEqual` Action.Update ((entry "" "" [] [textEntry "a"]) |> Entry.childZipper (Core.Array.firstZipper Entry.textZipper))
  , test "can delete the last child" <|
    Entry.delete ((entry "" "" [] [textEntry "a"]) |> Entry.childZipper (Core.Array.firstZipper Entry.textZipper))
      `assertEqual` Action.Update ((entry "" "" [] []) |> Entry.textZipper)
  ]

promoteTest = Suite "promote" <|
  [ test "moves current inbox item to children" <|
    Entry.promote ((entry "" "" [textEntry "a",textEntry "b"] []) |> Entry.inboxZipper (Core.Array.firstZipper Entry.textZipper))
    `assertEqual` Action.Update ((entry "" "" [textEntry "b"] [textEntry "a"]) |> Entry.inboxZipper (Core.Array.firstZipper Entry.textZipper))
  , test "makes the new item the first child" <|
    Entry.promote ((entry "" "" [textEntry "a",textEntry "b"] [textEntry "x"]) |> Entry.inboxZipper (Core.Array.firstZipper Entry.textZipper))
    `assertEqual` Action.Update ((entry "" "" [textEntry "b"] [textEntry "a", textEntry "x"]) |> Entry.inboxZipper (Core.Array.firstZipper Entry.textZipper))
  , test "with terminal inbox item, moves cursor up" <|
    Entry.promote ((entry "" "" [textEntry "a",textEntry "b"] [textEntry "x"]) |> Entry.inboxZipper (Core.Array.lastZipper Entry.textZipper))
    `assertEqual` Action.Update ((entry "" "" [textEntry "a"] [textEntry "b", textEntry "x"]) |> Entry.inboxZipper (Core.Array.firstZipper Entry.textZipper))
  , test "with the last inbox item, cursor follows to children" <|
    Entry.promote ((entry "" "" [textEntry "a"] [textEntry "x"]) |> Entry.inboxZipper (Core.Array.firstZipper Entry.textZipper))
    `assertEqual` Action.Update ((entry "" "" [] [textEntry "a", textEntry "x"]) |> Entry.childZipper (Core.Array.firstZipper Entry.textZipper))
  ]

moveIntoTest = Suite "moveInto" <|
  [ test "moves current inbox item to inbox of specified child" <|
    Entry.moveInto 1
      (Entry.inboxZipper (Core.Array.firstZipper Entry.textZipper) (entry "" "" [textEntry "a",textEntry "b"] [textEntry "0", textEntry "1"]))
    `assertEqual` Action.Update (Entry.inboxZipper (Core.Array.firstZipper Entry.textZipper) (entry "" "" [textEntry "b"] [textEntry "0", entry "1" "" [textEntry "a"] []]))
  , test "when specified child doesn't exist, does nothing" <|
    Entry.moveInto 5 (Entry.inboxZipper (Core.Array.firstZipper Entry.textZipper) (entry "" "" [textEntry "a",textEntry "b"] [entry "0" "" [] [], entry "1" "" [] []]))
      `assertEqual` Action.NoChange
  , test "moving last inbox item moves cursor" <|
    Entry.moveInto 0 (Entry.inboxZipper (Core.Array.firstZipper Entry.textZipper) (entry "" "" [textEntry "a"] [textEntry "0", textEntry "1"]))
      `assertEqual` Action.Update (Entry.childZipper (Core.Array.firstZipper (Entry.inboxZipper (Core.Array.firstZipper Entry.textZipper))) (entry "" "" [] [entry "0" "" [textEntry "a"] [], textEntry "1"]))
  , test "moving last inbox item when first child's inbox is empty" <|
    Entry.moveInto 1 (Entry.inboxZipper (Core.Array.firstZipper Entry.textZipper) (entry "" "" [textEntry "a"] [entry "0" "" [] [], entry "1" "" [] []]))
      `assertEqual` Action.Update (Entry.childZipper (Core.Array.lastZipper (Entry.inboxZipper (Core.Array.firstZipper Entry.textZipper))) (entry "" "" [] [entry "0" "" [] [], entry "1" "" [textEntry "a"] []]))
  ]

missortTest = Suite "missort" <|
  [ test "moves inbox item to parent's inbox" <|
    Entry.missort ((entry "" "" [] [entry "" "" [textEntry "a",textEntry "b"] []]) |> Entry.childZipperAt 0 (Entry.inboxZipperAt 0 Entry.textZipper))
    `assertEqual`
    Action.Update ((entry "" "" [textEntry "a"] [entry "" "" [textEntry "b"] []]) |> Entry.childZipperAt 0 (Entry.inboxZipperAt 0 Entry.textZipper))
  , test "works in nested children" <|
    Entry.missort ((entry "" "" [] [entry "" "" [] [entry "" "" [textEntry "a",textEntry "b"] []]]) |> Entry.childZipperAt 0 (Entry.childZipperAt 0 (Entry.inboxZipperAt 0 Entry.textZipper)))
    `assertEqual`
    Action.Update ((entry "" "" [] [entry "" "" [textEntry "a"] [entry "" "" [textEntry "b"] []]]) |> Entry.childZipperAt 0 (Entry.childZipperAt 0 (Entry.inboxZipperAt 0 Entry.textZipper)))
  , test "moves child to parent's inbox" <|
    Entry.missort ((entry "" "" [] [entry "" "" [] [textEntry "a", textEntry "b"]]) |> Entry.childZipperAt 0 (Entry.childZipperAt 0 Entry.textZipper))
    `assertEqual`
    Action.Update ((entry "" "" [textEntry "a"] [entry "" "" [] [textEntry "b"]]) |> Entry.childZipperAt 0 (Entry.childZipperAt 0 Entry.textZipper))
  ]

moveChildTest = Suite "moveChild" <|
  [ test "move child up" <|
    Entry.moveChildUp ((entry "" "" [] (map textEntry ["a","b","c","d"])) |> Entry.childZipperAt 2 Entry.textZipper)
    `assertEqual`
    Action.Update ((entry "" "" [] (map textEntry ["a","c","b","d"])) |> Entry.childZipperAt 1 Entry.textZipper)
  , test "move child up does nothing at first child" <|
    Entry.moveChildUp ((entry "" "" [] (map textEntry ["a","b","c","d"])) |> Entry.childZipperAt 0 Entry.textZipper)
    `assertEqual`
    Action.Update ((entry "" "" [] (map textEntry ["a","b","c","d"])) |> Entry.childZipperAt 0 Entry.textZipper)
  , test "move child down" <|
    Entry.moveChildDown ((entry "" "" [] (map textEntry ["a","b","c","d"])) |> Entry.childZipperAt 2 Entry.textZipper)
    `assertEqual`
    Action.Update ((entry "" "" [] (map textEntry ["a","b","d","c"])) |> Entry.childZipperAt 3 Entry.textZipper)
  , test "move child down does nothing at last child" <|
    Entry.moveChildDown ((entry "" "" [] (map textEntry ["a","b","c","d"])) |> Entry.childZipperAt 3 Entry.textZipper)
    `assertEqual`
    Action.Update ((entry "" "" [] (map textEntry ["a","b","c","d"])) |> Entry.childZipperAt 3 Entry.textZipper)
  , test "move child when in an inbox does nothing" <|
    Entry.moveChildDown ((entry "" "" [] (map (\e -> entry e "" [textEntry e] []) ["a","b","c","d"])) |> Entry.childZipperAt 0 (Entry.inboxZipper (Core.Array.firstZipper Entry.textZipper)))
    `assertEqual`
    Action.NoChange
  ]

suite = Suite "Outline.Entry"
  [ navTest
  , editTest
  , backspaceTest
  , enterTest
  , deleteTest
  , promoteTest
  , moveIntoTest
  , missortTest
  , moveChildTest
  ]
