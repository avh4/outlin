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
import Core.Array
import Outline.Scratch.Json as Scratch

---- SIGNALS

dropbox = Dropbox.client "sy8pzlg66rwnv7n"

tabsChannel = Signal.channel ""
scratchChannel = Signal.channel 0

commands : Signal Command
commands = Signal.mergeMany
  [ Signal.map Key Keys.lastPressed
  , Signal.map Tab (Signal.subscribe tabsChannel)
  , Signal.map Scratch (Signal.subscribe scratchChannel)
  , Signal.map LoadedOutline <| dropbox.read "outlin.json"
  , Signal.map LoadedScratch <| dropbox.read "scratch.json"
  ]

initialDocument = (Document.scratchZipper 0 SampleData.template)

state = Signal.foldp App.step initialDocument commands

---- OUTPUT SIGNALS

main = Signal.map2 (App.render tabsChannel scratchChannel) Window.dimensions state

outlineOutput = Signal.dropRepeats <| Signal.map (\x -> x |> Document.outlineValue |> Entry.toJson) state

scratchOutput = Signal.dropRepeats <| Signal.map (\x -> x |> Document.scratchValue |> Core.Array.toJson Scratch.toJson) state

outlineToDropbox = dropbox.write "outlin.json" outlineOutput
scratchToDropbox = dropbox.write "scratch.json" scratchOutput
