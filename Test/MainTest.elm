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
    , Key (Character "By time")
    , Key (Enter)
    , Key (Character "Habits")
    , Key (Command "p") -- promote
    , Key (Command "p")
    ]
  `assertEqual`
  App.Model
    (entry "Tasks" "" []
      [ entry "By time" "" [] []
      , entry "Habits" "" [] []
      ])
    (InChild (0,InText 6))

suite = Suite "Integration tests" [test1]