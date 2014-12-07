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
import App.Render.Main as App
import Outline.Document.Model as Document
import Outline.Document.Json as Document
import Signal
import Debug

---- SIGNALS

-- dropbox = Dropbox.client "mjplyqeks6z3js8"

tabsChannel = Signal.channel ""
scratchChannel = Signal.channel 0

commands : Signal Command
commands = Signal.mergeMany
  [ Signal.map Key Keys.lastPressed
  , Signal.map Tab (Signal.subscribe tabsChannel)
  , Signal.map Scratch (Signal.subscribe scratchChannel)
  -- , Signal.map Loaded <| dropbox.read "outlin.json"
  ]

initialDocument = (Document.scratchZipper 0 SampleData.template)

state = Signal.foldp App.step initialDocument commands

---- OUTPUT SIGNALS

main = Signal.map2 (App.render tabsChannel scratchChannel) Window.dimensions state

jsonOutput = Signal.dropRepeats <| Signal.map (\x -> x |> Document.toValue |> Document.toJson) state

-- toDropbox = dropbox.write "outlin.json" jsonOutput
