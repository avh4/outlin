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
import Json.Decode

---- SIGNALS

dropbox = Dropbox.client "sy8pzlg66rwnv7n"

tabsChannel = Signal.channel ""
scratchChannel = Signal.channel 0
processScratchChannel = Signal.channel ()
newScratchChannel = Signal.channel ()

decode : Json.Decode.Decoder x -> String -> Result String x
decode decoder s = Json.Decode.decodeString decoder s

fromDropbox : String -> Json.Decode.Decoder x -> (Result String x -> Command) -> Signal Command
fromDropbox filename decoder fn =
  dropbox.read filename
  |> Signal.map (decode decoder)
  |> Signal.map fn

commands : Signal Command
commands = Signal.mergeMany
  [ Signal.map Key Keys.lastPressed
  , Signal.map Paste Keys.pastes
  , Signal.map Tab (Signal.subscribe tabsChannel)
  , Signal.map Scratch (Signal.subscribe scratchChannel)
  , Signal.map (\_ -> ProcessScratch) (Signal.subscribe processScratchChannel)
  , Signal.map (\_ -> NewScratch) (Signal.subscribe newScratchChannel)
  , fromDropbox "outlin.json" Entry.decoder LoadedOutline
  , fromDropbox "scratch.json" Scratch.listDecoder LoadedScratch
  ]

-- initialDocument = (Document.scratchZipper 0 SampleData.template)
initialDocument = Document.emptyValue |> Document.scratchZipper 0

state = Signal.foldp App.step initialDocument commands

---- OUTPUT SIGNALS

main = Signal.map2 (App.render tabsChannel scratchChannel processScratchChannel newScratchChannel) state Window.dimensions

outlineOutput = Signal.dropRepeats <| Signal.map (\x -> x |> Document.toValue |> .outline |> Entry.toJson) state

scratchOutput = Signal.dropRepeats <| Signal.map (\x -> x |> Document.toValue |> .scratch |> Core.Array.toJson Scratch.toJson) state

outlineToDropbox = dropbox.write "outlin.json" outlineOutput
scratchToDropbox = dropbox.write "scratch.json" scratchOutput
