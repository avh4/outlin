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

dropbox = Dropbox.client "mjplyqeks6z3js8"

tabsChannel = Signal.channel ""
scratchChannel = Signal.channel 0

decode : Json.Decode.Decoder x -> String -> Maybe x
decode decoder s = case Json.Decode.decodeString decoder s of
  Ok doc -> Just doc
  x -> fst (Nothing, Debug.log "Load failed" x)

fromDropbox : String -> Json.Decode.Decoder x -> (Maybe x -> Command)
fromDropbox filename decoder fn =
  dropbox.read filename
  |> decode decoder
  |> Signal.map fn

commands : Signal Command
commands = Signal.mergeMany
  [ Signal.map Key Keys.lastPressed
  , Signal.map Paste Keys.pastes
  , Signal.map Tab (Signal.subscribe tabsChannel)
  , Signal.map Scratch (Signal.subscribe scratchChannel)
  , fromDropbox "outlin.json" Entry.decoder LoadedOutline
  , fromDropbox "scratch.json" Scratch.listDecoder LoadedScratch
  ]

initialDocument = (Document.scratchZipper 0 SampleData.template)

state = Signal.foldp App.step initialDocument commands

---- OUTPUT SIGNALS

main = Signal.map2 (App.render tabsChannel scratchChannel) Window.dimensions state

outlineOutput = Signal.dropRepeats <| Signal.map (\x -> x |> Document.outlineValue |> Entry.toJson) state

scratchOutput = Signal.dropRepeats <| Signal.map (\x -> x |> Document.scratchValue |> Core.Array.toJson Scratch.toJson) state

outlineToDropbox = dropbox.write "outlin.json" outlineOutput
scratchToDropbox = dropbox.write "scratch.json" scratchOutput
