module Test.MainTest where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

import App
import App (Command(..))
import Keys (KeyInput(..))
import Outline.Entry (entry, BaseCursor(..))
import Outline.Document as Document

test1 = test "first-use scenario" <|
  foldl App.step (Document.Zipper (entry "" "" [] []) (InText 0))
    [ Key (Character "Tasks")
    , Key (Command "a") -- add
    , Key (Character "By time")
    , Key (Enter)
    , Key (Character "Habits")
    , Key (Command "p") -- promote
    , Key (Command "p")
    ]
  `assertEqual`
  Document.Zipper
    (entry "Tasks" "" []
      [ entry "By time" "" [] []
      , entry "Habits" "" [] []
      ])
    (InChild (0,InText 0))

test2 = test "sorting in an empty template" <|
  foldl App.step (Document.Zipper
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
      ) (InText 5))
    [ Key (Command "a") -- add
    , Key (Character "Watch Strange Loop videos")
    , Key (Enter)
    , Key (Character "Read Illustrated guide to objc_msgSend")
    , Key (Enter)
    , Key (Character "get volunteers for Girl Develop It")
    , Key (Enter)
    , Key (Character "Read voting guide")
    , Key (Command "a")
    , Key (Character "get foam mattress topper")
    , Key (Command "3") -- mattress topper -> priority
    , Key (Command "4") -- Strange Loop -> project
    , Key (Command "4") -- objc_msgSend -> project
    , Key (Command "1") -- Girl Develop It -> time
    , Key (Command "1") -- voting guide -> time
    ]
  `assertEqual`
  Document.Zipper
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
    (InChild (0,InInbox (0,InText 9)))

suite = Suite "Integration tests"
  [ test1
  , test2
  ]