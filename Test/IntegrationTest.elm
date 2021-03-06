module Test.IntegrationTest where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

import App
import App.Command (..)
import Keys (..)
import Outline.Entry (entry,Outline(..),Zipper(..))
import Outline.Entry as Entry
import Outline.Document.Model as Document
import Outline.Scratch.Model as Scratch
import Core.String
import Core.Array
import List (foldl)
import RichText
import Outline.Document.State as State

initialDocument = State.InScratch Document.emptyValue

assertEqualTasks : State.State -> Entry.Zipper -> Assertion
assertEqualTasks state entry = assertEqual (state |> State.toDocument |> .tasks) entry

test1 = test "first-use scenario" <|
  foldl App.step initialDocument
    [ Tab "Tasks"
    , Key (Character "Tasks")
    , Key (CommandCharacter"a") -- add
    , Key (Character "By time")
    , Key (Single Enter)
    , Key (Character "Habits")
    , Key (CommandCharacter "p") -- promote
    , Key (CommandCharacter "p")
    ]
  `assertEqualTasks`
  Entry.childZipper (Core.Array.firstZipper Entry.textZipper)
    ( entry "Tasks" "" []
      [ entry "By time" "" [] []
      , entry "Habits" "" [] []
      ]
    )

test2 = test "sorting in an empty template" <|
  foldl App.step initialDocument
    [ Tab "Tasks"
    , LoadedTasks (Ok
      ( entry "Tasks" "" []
        [ entry "By time" "" []
          [ entry "daily" "" [] []
          , entry "weekly" "" [] []
          , entry "waiting on" "" [] []
          , entry "monthly" "" [] []
          , entry "yearly" "" [] []
          ]
        , entry "Habits" "" []
          [ entry "daily" "" [] []
          , entry "weekly" "" [] []
          , entry "monthly" "" [] []
          ]
        , entry "By priority" "" [] []
        , entry "By projet" "" [] []
        ]
      ))
    , Key (CommandCharacter "a") -- add
    , Key (Character "Watch Strange Loop videos")
    , Key (Single Enter)
    , Key (Character "Read Illustrated guide to objc_msgSend")
    , Key (Single Enter)
    , Key (Character "get volunteers for Girl Develop It")
    , Key (Single Enter)
    , Key (Character "Read voting guide")
    , Key (CommandCharacter "a")
    , Key (Character "get foam mattress topper")
    , Key (CommandCharacter "3") -- mattress topper -> priority
    , Key (CommandCharacter "4") -- Strange Loop -> project
    , Key (CommandCharacter "4") -- objc_msgSend -> project
    , Key (CommandCharacter "1") -- Girl Develop It -> time
    , Key (CommandCharacter "1") -- voting guide -> time
    ]
  `assertEqualTasks`
  Entry.childZipper (Core.Array.firstZipper (Entry.inboxZipper (Core.Array.firstZipper Entry.textZipper)))
    ( entry "Tasks" "" []
      [ entry "By time" ""
        [ entry "Read voting guide" "" [] []
        , entry "get volunteers for Girl Develop It" "" [] []
        ]
        [ entry "daily" "" [] []
        , entry "weekly" "" [] []
        , entry "waiting on" "" [] []
        , entry "monthly" "" [] []
        , entry "yearly" "" [] []
        ]
      , entry "Habits" "" []
        [ entry "daily" "" [] []
        , entry "weekly" "" [] []
        , entry "monthly" "" [] []
        ]
      , entry "By priority" ""
        [ entry "get foam mattress topper" "" [] []
        ]
        []
      , entry "By projet" ""
        [ entry "Read Illustrated guide to objc_msgSend" "" [] []
        , entry "Watch Strange Loop videos" "" [] []
        ]
        []
      ]
    )

test3 =
  case foldl App.step initialDocument
      [ Tab "Scratch"
      , Key (CommandShift Left)
      , Key (Character "Weekly review")
      , Key (Single Enter)
      , Key (Character "review finances")
      , Key (CommandShift Left)
      , Key (CommandCharacter "b") -- mark task
      , Key (Command Right)
      , Key (Single Enter)
      , Key (Character "book flight to Toronto")
      , Key (CommandShift Left)
      , Key (CommandCharacter "b") -- mark task
      , ProcessScratch
      ] of
  result ->
    Suite "Processing scratch files"
    [ test "adds tasks" <|
      (result |> State.toDocument |> .tasks |> Entry.toValue)
      `assertEqual`
      entry "" ""
      [ entry "review finances" "" [] []
      , entry "book flight to Toronto" "" [] []
      ] []
    , test "removes processed scratch" <|
      (result |> State.toDocument |> .scratch |> Core.Array.toValue Scratch.toValue)
      `assertEqual`
      [[RichText.heading "Scratch 1"]]
    , test "archives processed scratch to notes" <|
      (result |> State.toDocument |> .notes)
      `assertEqual`
      [
        [ RichText.heading "Weekly review"
        , (RichText.Task, [RichText.span "review finances"])
        , (RichText.Task, [RichText.span "book flight to Toronto"])
        ]
      ]
    -- , test "archives tasks"
    ]

suite = Suite "Integration tests"
  [ test1
  , test2
  , test3
  ]
