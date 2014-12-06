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
import Signal
import Debug

---- SIGNALS

dropbox = Dropbox.client "sy8pzlg66rwnv7n"

commands : Signal Command
commands = Signal.mergeMany
  [ Signal.map Key Keys.lastPressed
  , Signal.map Loaded <| dropbox.read "outlin.json"
  ]

initialDocument = (Entry.textZipper SampleData.template)

state = Signal.foldp App.step initialDocument commands

---- OUTPUT SIGNALS

main = Signal.map2 App.render Window.dimensions state

jsonOutput = Signal.dropRepeats <| Signal.map (\x -> x |> Entry.toValue |> Entry.toJson) state

toDropbox = dropbox.write "outlin.json" jsonOutput
