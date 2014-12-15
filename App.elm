module App where

import Html (Html, text, node, toElement)
import Html.Attributes (class)

import String
import Keys
import Char
import Debug
import Outline.Entry as Entry
import Json.Decode
import Json.Encode
import Core.Action as Action
import Core.String
import Core.Array
import Color (..)
import Text
import Outline.Document.Model as Document
import Outline.Document.Actions as Document
import Outline.Scratch.Model as Scratch
import Outline.Scratch.Json as Scratch
import Outline.RichText.Span.Model as Span
import Outline.RichText.Span.Actions as Span
import App.EntryNav as EntryNav
import Graphics.Element (flow, right, down, Element, width, heightOf, widthOf, spacer, color, container, topLeft, midLeft)
import Graphics.Collage (collage, toForm, rotate)
import Text (plainText)
import List
import List (..)

---- App

updateValue : (Document.Value -> Document.Zipper) -> Document.Zipper -> Document.Zipper
updateValue toZipper z = z |> Document.toValue |> toZipper

updateZipper : (Document.Zipper -> Document.Result) -> Document.Zipper -> Document.Zipper
updateZipper action z = case action z of
  Action.Update z' -> z'
  -- explicity list the following action results, which are all no-ops on document
  Action.Split _ _ _ -> z
  Action.Delete -> z
  Action.EnterNext -> z
  Action.EnterPrev -> z
  Action.NoChange -> z

updateEntry action = updateZipper (Document.doEntry action)
updateText action = updateZipper (Document.doText action)
updateSpan action = updateZipper (Document.doSpan action)

---- INPUT

type Command
  = Key Keys.KeyCombo
  | Paste String
  | LoadedOutline (Result String Entry.Value)
  | LoadedScratch (Result String (List Scratch.Value))
  | Tab String
  | Scratch Int

-- TODO: refactor to keep trailing 'm' out of here
step : Command -> Document.Zipper -> Document.Zipper
step c m = case c of
  Key (Keys.Single (Keys.Left)) -> updateText Core.String.goLeft m
  Key (Keys.Single (Keys.Right)) -> updateText Core.String.goRight m
  Key (Keys.Single (Keys.Down)) -> updateEntry (Entry.doEntry EntryNav.goDownWithinChild) m
  Key (Keys.Single (Keys.Up)) -> updateEntry (Entry.doEntry EntryNav.goUpWithinChild) m
  Key (Keys.Single (Keys.Enter)) -> updateZipper Document.enter m
  Key (Keys.CommandCharacter "a") -> updateEntry Entry.addInboxItem m
  Key (Keys.CommandCharacter "d") -> updateText Core.String.delete m
  Key (Keys.CommandCharacter "m") -> updateEntry Entry.missort m
  Key (Keys.CommandCharacter "p") -> updateEntry Entry.promote m
  Key (Keys.CommandCharacter "1") -> updateEntry (Entry.moveInto 0) m
  Key (Keys.CommandCharacter "2") -> updateEntry (Entry.moveInto 1) m
  Key (Keys.CommandCharacter "3") -> updateEntry (Entry.moveInto 2) m
  Key (Keys.CommandCharacter "4") -> updateEntry (Entry.moveInto 3) m
  Key (Keys.CommandCharacter "5") -> updateEntry (Entry.moveInto 4) m
  Key (Keys.CommandCharacter "6") -> updateEntry (Entry.moveInto 5) m
  Key (Keys.CommandCharacter "7") -> updateEntry (Entry.moveInto 6) m
  Key (Keys.Alt (Keys.Up)) -> updateEntry (Entry.doEntry EntryNav.goToPrevSibling) m
  Key (Keys.Alt (Keys.Down)) -> updateEntry (Entry.doEntry EntryNav.goToNextSibling) m
  Key (Keys.Alt (Keys.Right)) -> updateEntry EntryNav.goToFirstChild m
  Key (Keys.Alt (Keys.Left)) -> updateEntry EntryNav.goToParent m
  Key (Keys.Command (Keys.Up)) -> updateEntry Entry.moveChildUp m
  Key (Keys.Command (Keys.Down)) -> updateEntry Entry.moveChildDown m

  -- Text
  Key (Keys.Single (Keys.Backspace)) -> updateText Core.String.backspace m
  Key (Keys.Character s) -> updateText (Core.String.insert s) m
  Paste s -> updateText (Core.String.insert s) m

  -- Selection
  Key (Keys.Shift (Keys.Left)) -> updateText Core.String.selectLeft m
  Key (Keys.Shift (Keys.Right)) -> updateText Core.String.selectRight m
  Key (Keys.CommandShift (Keys.Left)) -> updateText Core.String.selectToStart m
  Key (Keys.CommandShift (Keys.Right)) -> updateText Core.String.selectToEnd m

  -- Formatting
  Key (Keys.CommandCharacter "b") -> updateSpan (Span.applyStyle Span.Bold) m

  Tab "Scratch" -> updateValue (Document.scratchZipper 0) m
  Tab "Tasks" -> updateValue Document.outlineZipper m

  Scratch i -> updateValue (Document.scratchZipper i) m

  LoadedOutline (Ok e) -> Document.replaceOutline m e
  LoadedScratch (Ok s) -> Document.replaceScratch m s

  x -> fst (m, Debug.log "Unhandled command" x)
