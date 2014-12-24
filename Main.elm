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
import Outline.Notes.Json as Notes
import List

---- SIGNALS

dropbox = Dropbox.client "sy8pzlg66rwnv7n"

tabsChannel = Signal.channel ""
scratchChannel = Signal.channel 0
processScratchChannel = Signal.channel ()
newScratchChannel = Signal.channel ()

decode : Json.Decode.Decoder x -> String -> Result String x
decode decoder s = Json.Decode.decodeString decoder s

type alias DropboxChannel = (String, (String -> Command), (Document.Value -> String))

fromDropbox : DropboxChannel -> Signal Command
fromDropbox (filename, fn, _) =
  dropbox.read filename
  |> Signal.map fn

dch : String -> Json.Decode.Decoder a -> (Result String a -> Command) -> (Document.Value -> b) -> (b -> String) -> DropboxChannel
dch filename decoder fn fromDoc toJson =
  ( filename
  , (\x -> decode decoder x |> fn)
  , (\x -> x |> fromDoc |> toJson)
  )

dropboxChannels : List DropboxChannel
dropboxChannels =
  [ dch "outlin.json" Entry.decoder LoadedOutline .outline Entry.toJson
  , dch "scratch.json" Scratch.listDecoder LoadedScratch .scratch (Core.Array.toJson Scratch.toJson)
  , dch "notes.json" Notes.decoder LoadedNotes .notes Notes.toJson
  ]

toDropbox (filename, _, fn) = state
  |> Signal.map Document.toValue
  |> Signal.dropRepeats
  |> Signal.map fn
  |> dropbox.write filename

commands : Signal Command
commands = Signal.mergeMany
  [ Signal.map Key Keys.lastPressed
  , Signal.map Paste Keys.pastes
  , Signal.map Tab (Signal.subscribe tabsChannel)
  , Signal.map Scratch (Signal.subscribe scratchChannel)
  , Signal.map (\_ -> ProcessScratch) (Signal.subscribe processScratchChannel)
  , Signal.map (\_ -> NewScratch) (Signal.subscribe newScratchChannel)
  , Signal.mergeMany <| List.map fromDropbox dropboxChannels
  ]

-- initialDocument = (Document.scratchZipper 0 SampleData.template)
initialDocument = Document.emptyValue |> Document.scratchZipper 0

state = Signal.foldp App.step initialDocument commands

---- OUTPUT SIGNALS

main = Signal.map2 (App.render tabsChannel scratchChannel processScratchChannel newScratchChannel) state Window.dimensions

dropboxOutputs = List.map toDropbox dropboxChannels
