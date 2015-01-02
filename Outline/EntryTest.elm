module Outline.EntryTest where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

import Core.Action as Action
import Outline.Entry as Entry
import Outline.Entry (entry,BaseZipper(..))
import Core.Array
import Core.String
import List (map)

textEntry s = entry s "" [] []

simpleTree = (entry "root" "" [] [textEntry "a", textEntry "b"])

backspace = Entry.do Core.String.backspace
enter = Entry.do Core.String.split
goLeft = Entry.do Core.String.goLeft
goRight = Entry.do Core.String.goRight
goNext = Entry.do (\_ -> Action.EnterNext)
goPrev = Entry.do (\_ -> Action.EnterPrev)
delete = Entry.do (\_ -> Action.Delete)

backspaceTest : Test
backspaceTest = Suite "backspace"
  [ backspace (textEntry "Elm" |> Entry.textZipperAt 1)
      `equals` Action.Update (textEntry "lm" |> Entry.textZipperAt 0)
  ]

navTest = Suite "navigation"
  [ Suite "text"
    [ test "can go left in text" <|
      goLeft (textEntry "ab" |> Entry.textZipperAt 1)
        `assertEqual` Action.Update (textEntry "ab" |> Entry.textZipperAt 0)
    , test "go left stops at the edge" <|
      goLeft ((textEntry "ab") |> Entry.textZipperAt 0)
        `assertEqual` Action.EnterPrev -- TODO: should be Action.NoChange
    , test "can go right in text" <|
      goRight ((textEntry "ab") |> Entry.textZipperAt 1)
        `assertEqual` Action.Update ((textEntry "ab") |> Entry.textZipperAt 2)
    , test "go right stops at the edge" <|
      goRight ((textEntry "ab") |> Entry.textZipperAt 2)
        `assertEqual` Action.EnterNext -- TODO: should be Action.NoChange
    ]
  , Suite "children"
    [ test "can go to next child" <|
      goNext (Entry.childZipper (Core.Array.firstZipper Entry.textZipper) simpleTree)
        `assertEqual` Action.Update (Entry.childZipper (Core.Array.lastZipper Entry.textZipper) simpleTree)
    , test "can go to prev child" <|
      goPrev (simpleTree |> (Entry.childZipper <| Core.Array.lastZipper Entry.textZipper))
        `assertEqual` Action.Update (simpleTree |> (Entry.childZipper <| Core.Array.firstZipper Entry.textZipper))
    , test "can go into child" <|
      goNext (simpleTree |> Entry.textZipper)
        `assertEqual` Action.Update (simpleTree |> (Entry.childZipper <| Core.Array.firstZipper Entry.textZipper))
    , test "can go out of child" <|
      goPrev (simpleTree |> (Entry.childZipper <| Core.Array.firstZipper Entry.textZipper))
        `assertEqual` Action.Update (simpleTree |> Entry.textZipper)
    , test "can go to next parent" <|
      let tree = (entry "" "" [] [entry "" "" [] [textEntry "a"], entry "" "" [] []])
      in goNext (tree |> (Entry.childZipper (Core.Array.firstZipper (Entry.childZipper <| Core.Array.firstZipper Entry.textZipper))))
        `assertEqual` Action.Update (tree |> (Entry.childZipper <| Core.Array.lastZipper Entry.textZipper))
    , test "can go to child of previous parent" <|
      let tree = (entry "" "" [] [entry "" "" [] [textEntry "a"], entry "" "" [] []])
      in goPrev (tree |> Entry.childZipper (Core.Array.lastZipper Entry.textZipper))
        `assertEqual` Action.Update (tree |> Entry.childZipper (Core.Array.firstZipper (Entry.childZipper (Core.Array.firstZipper Entry.textZipper))))
    ]
  , Suite "inbox" <|
    let tree = (entry "" "" [textEntry "a",textEntry "b"] [])
    in
    [ test "can go to next inbox item" <|
      goNext (tree |> Entry.inboxZipper (Core.Array.firstZipper Entry.textZipper))
        `assertEqual` Action.Update (tree |> Entry.inboxZipper (Core.Array.lastZipper Entry.textZipper))
    , test "can enter inbox item" <|
      goNext (tree |> Entry.textZipper)
        `assertEqual` Action.Update (tree |> Entry.inboxZipper (Core.Array.firstZipper Entry.textZipper))
    , test "can exit last inbox item into children" <|
      goNext ((entry "" "" [textEntry "a",textEntry "b"] [textEntry "x"]) |> Entry.inboxZipper (Core.Array.lastZipper Entry.textZipper))
        `assertEqual` Action.Update ((entry "" "" [textEntry "a",textEntry "b"] [textEntry "x"]) |> Entry.childZipper (Core.Array.firstZipper Entry.textZipper))
    , test "can exit last inbox item with no children" <|
      goNext (tree |> Entry.inboxZipper (Core.Array.lastZipper Entry.textZipper))
        `assertEqual` Action.EnterNext
    , test "can go to prev inbox item" <|
      goPrev (tree |> Entry.inboxZipper (Core.Array.lastZipper Entry.textZipper))
        `assertEqual` Action.Update (tree |> Entry.inboxZipper (Core.Array.firstZipper Entry.textZipper))
    , test "can enter inbox from bottom" <|
      goPrev ((entry "" "" [textEntry "a",textEntry "b"] [textEntry "x"]) |> Entry.childZipper (Core.Array.firstZipper Entry.textZipper))
        `assertEqual` Action.Update ((entry "" "" [textEntry "a",textEntry "b"] [textEntry "x"]) |> Entry.inboxZipper (Core.Array.lastZipper Entry.textZipper))
    , test "can exit first inbox item" <|
      goPrev (tree |> Entry.inboxZipper (Core.Array.firstZipper Entry.textZipper))
        `assertEqual` Action.Update (tree |> Entry.textZipper)
    ]
  ]

editTest = Suite "basic editing"
  [ test "can insert text" <|
    Entry.do (Core.String.insert "xx") ((textEntry "ab") |> Entry.textZipperAt 1)
      `assertEqual` Action.Update ((textEntry "axxb") |> Entry.textZipperAt 3)
  , test "can backspace text" <|
    backspace ((textEntry "Elm") |> Entry.textZipperAt 1)
      `assertEqual` Action.Update ((textEntry "lm") |> Entry.textZipperAt 0)
  , test "backspace stops at edge" <|
    backspace ((textEntry "Elm") |> Entry.textZipperAt 0)
      `assertEqual` Action.NoChange
  ]

enterTest = Suite "enter"
  [ test "can split an Entry" <|
    enter ((textEntry "ab") |> Entry.textZipperAt 1)
      `assertEqual` Action.Split [textEntry "a"] (textEntry "b" |> Entry.textZipperAt 0) []
  , test "can split a child Entry" <|
    enter ((entry "" "" [] [textEntry "ab"]) |> Entry.childZipper (Core.Array.firstZipper <| Entry.textZipperAt 1))
      `assertEqual` Action.Update ((entry "" "" [] [textEntry "a", textEntry "b"]) |> Entry.childZipper (Core.Array.lastZipper <| Entry.textZipperAt 0))
  ]

deleteTest = Suite "delete"
  [ test "can delete an inbox item" <|
    delete ((entry "" "" [textEntry "a", textEntry "b"] []) |> Entry.inboxZipper (Core.Array.firstZipper Entry.textZipper))
      `assertEqual` Action.Update ((entry "" "" [textEntry "b"] []) |> Entry.inboxZipper (Core.Array.firstZipper Entry.textZipper))
  , test "can delete the last inbox item" <|
    delete ((entry "" "" [textEntry "a"] []) |> Entry.inboxZipper (Core.Array.firstZipper Entry.textZipper))
      `assertEqual` Action.Update ((entry "" "" [] []) |> Entry.textZipper)
  , test "can delete a child" <|
    delete ((entry "" "" [] [textEntry "a", textEntry "b"]) |> Entry.childZipper (Core.Array.firstZipper Entry.textZipper))
      `assertEqual` Action.Update ((entry "" "" [] [textEntry "b"]) |> Entry.childZipper (Core.Array.firstZipper Entry.textZipper))
  , test "can delete the terminal child" <|
    delete ((entry "" "" [] [textEntry "a", textEntry "b"]) |> Entry.childZipper (Core.Array.lastZipper Entry.textZipper))
      `assertEqual` Action.Update ((entry "" "" [] [textEntry "a"]) |> Entry.childZipper (Core.Array.firstZipper Entry.textZipper))
  , test "can delete the last child" <|
    delete ((entry "" "" [] [textEntry "a"]) |> Entry.childZipper (Core.Array.firstZipper Entry.textZipper))
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
