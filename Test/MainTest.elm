module Test.MainTest where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

import App
import App (Command(..))
import Keys (..)
import Outline.Entry (entry,BaseValue(..),BaseZipper(..))
import Outline.Entry as Entry
import Outline.Document.Model as Document
import Core.String
import Core.Array
import List (foldl)

emptyDocument = Document.outlineZipper {scratch=[], outline=Entry.emptyEntry}

assertEqualOutline : Document.Zipper -> Entry.Zipper -> Assertion
assertEqualOutline doc entry = assertEqual doc (Document.InOutline [] entry)

test1 = test "first-use scenario" <|
  foldl App.step emptyDocument
    [ Key (Character "Tasks")
    , Key (CommandCharacter"a") -- add
    , Key (Character "By time")
    , Key (Single Enter)
    , Key (Character "Habits")
    , Key (CommandCharacter "p") -- promote
    , Key (CommandCharacter "p")
    ]
  `assertEqualOutline`
  Entry.childZipper (Core.Array.firstZipper Entry.textZipper)
    ( entry "Tasks" "" []
      [ entry "By time" "" [] []
      , entry "Habits" "" [] []
      ]
    )

test2 = test "sorting in an empty template" <|
  foldl App.step emptyDocument
    [ LoadedOutline (Just
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
  `assertEqualOutline`
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

suite = Suite "Integration tests"
  [ test1
  , test2
  ]
