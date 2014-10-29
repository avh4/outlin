module Main where

import Html (Html, text, node, toElement)
import Html.Attributes (class)

import Keys
import Outline.Entry as Entry
import SampleData

import App
import App (Command(..), Model)

---- SIGNALS

port dropboxIn : Signal String

commands : Signal Command
commands = merges
  [ Key <~ Keys.lastPressed
  , Loaded <~ dropboxIn
  ]

state = foldp App.step (Model SampleData.template (Entry.InText 4)) commands

---- OUTPUT SIGNALS

main = (toElement 800 600) <~ (App.render <~ state)

port dropboxOut : Signal String
port dropboxOut = dropRepeats <| (\x -> Entry.toJson x.value) <~ state
