module Main where

import Html (Html, text, node, toElement)
import Html.Attributes (class)

import Keys
import Dropbox
import Outline.Entry as Entry
import SampleData

import App
import App (Command(..))
import Outline.Document as Document

---- SIGNALS

dropbox = Dropbox.client "mjplyqeks6z3js8"

commands : Signal Command
commands = merges
  [ Key <~ Keys.lastPressed
  , Loaded <~ dropbox.read "outlin.json"
  ]

state = foldp App.step (Document.Zipper SampleData.template (Entry.InText 4)) commands

---- OUTPUT SIGNALS

main = (toElement 800 600) <~ (App.render <~ state)

jsonOutput = dropRepeats <| (\x -> Entry.toJson x.value) <~ state

toDropbox = dropbox.write "outlin.json" jsonOutput
