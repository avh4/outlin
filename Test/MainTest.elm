module Test.MainTest where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

import App
import App (Command(..))
import Keys (KeyInput(..))
import Outline.Entry (entry, BaseCursor(..))

test1 = test "first-use scenario" <|
  foldl App.step (App.Model (entry "" "" [] []) (InText 0))
    [ Key (Character "Tasks")
    , Key (Command "a") -- add
    , Key (Character "Inbox")
    , Key (Enter)
    , Key (Character "By time")
    , Key (Command "p") -- promote
    , Key (Command "p")
    ]
  `assertEqual`
  App.Model
    (entry "Tasks" "" []
      [ entry "Inbox" "" [] []
      , entry "By time" "" [] []
      ])
    (InChild (0,InText 7))

suite = Suite "Integration tests" [test1]