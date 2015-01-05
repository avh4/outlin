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
import App.Command (..)
import App.Render.Main as App
import Outline.Document.Model as Document
import Outline.Document.Json as Document
import Outline.Document.State as State
import Signal
import Debug
import Core.Array
import Outline.Scratch.Model as Scratch
import Outline.Scratch.Json as Scratch
import Json.Encode
import Json.Decode
import Outline.Notes.Json as Notes
import List

---- SIGNALS

dropbox = Dropbox.client "mjplyqeks6z3js8"

channel = Signal.channel Init_

decode : Json.Decode.Decoder x -> String -> Result String x
decode decoder s = Json.Decode.decodeString decoder s

type alias DropboxChannel = (String, (String -> Command), (Document.Document -> String))

fromDropbox : DropboxChannel -> Signal Command
fromDropbox (filename, fn, _) =
  dropbox.read filename
  |> Signal.map fn

dch : String -> Json.Decode.Decoder a -> (Result String a -> Command) -> (Document.Document -> Json.Encode.Value) -> DropboxChannel
dch filename decoder fn toJson =
  ( filename
  , (\x -> decode decoder x |> fn)
  , (\x -> x |> toJson |> Json.Encode.encode 0)
  )

dropboxChannels : List DropboxChannel
dropboxChannels =
  [ dch "outlin.json" Entry.decoder LoadedTasks (.tasks >> Entry.toValue >> Entry.toJson)
  , dch "scratch.json" Scratch.listDecoder LoadedScratch (.scratch >> Core.Array.toValue Scratch.toValue >> (\v -> List.map Scratch.toJson v |> Json.Encode.list))
  , dch "notes.json" Notes.decoder LoadedNotes (.notes >> Notes.toJson)
  ]

toDropbox (filename, _, fn) = state
  |> Signal.map State.toDocument
  |> Signal.dropRepeats
  |> Signal.map fn
  |> dropbox.write filename

commands : Signal Command
commands = Signal.mergeMany
  [ Signal.map Key Keys.lastPressed
  , Signal.map Paste Keys.pastes
  , Signal.subscribe channel
  , Signal.mergeMany <| List.map fromDropbox dropboxChannels
  ]

-- initialDocument = (Document.scratchZipper 0 SampleData.template)
initialDocument = State.InScratch Document.emptyValue

state = Signal.foldp App.step initialDocument commands

---- OUTPUT SIGNALS

main = Signal.map2 (App.render channel) state Window.dimensions

dropboxOutputs = List.map toDropbox dropboxChannels
