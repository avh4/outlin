module Main where

import Html (Html, text, node, toElement)
import Html.Attributes (class)

import Keys
import Dropbox
import Outline.Entry as Entry
import SampleData
import Window

import App
import App (Command(..), Model)

---- SIGNALS

dropbox = Dropbox.client "mjplyqeks6z3js8"

commands : Signal Command
commands = merges
  [ Key <~ Keys.lastPressed
  , Loaded <~ dropbox.read "outlin.json"
  ]

state = foldp App.step (Model SampleData.template (Entry.InText 4)) commands

---- OUTPUT SIGNALS

main = App.render <~ Window.dimensions ~ state

jsonOutput = dropRepeats <| (\x -> Entry.toJson x.value) <~ state

toDropbox = dropbox.write "outlin.json" jsonOutput
