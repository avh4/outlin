module Main where

import Html (Html, text, node, toElement)
import Html.Attributes (class)

import Keys
import Dropbox
import Outline.Entry as Entry
import Outline.Entry (entry)
import SampleData
import Window

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

initialDocument = (Entry.textZipper SampleData.template)

state = foldp App.step initialDocument commands

---- OUTPUT SIGNALS

main = App.render <~ Window.dimensions ~ state

jsonOutput = dropRepeats <| (\x -> x |> Entry.toValue |> Entry.toJson) <~ state

toDropbox = dropbox.write "outlin.json" jsonOutput
