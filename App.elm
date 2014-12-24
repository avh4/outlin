module App where

import Html (Html, text, node, toElement)
import Html.Attributes (class)

import String
import Keys (..)
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
import Outline.RichText.Block.Model as Block
import Outline.RichText.Block.Actions as Block
import App.EntryNav as EntryNav
import Graphics.Element (flow, right, down, Element, width, heightOf, widthOf, spacer, color, container, topLeft, midLeft)
import Graphics.Collage (collage, toForm, rotate)
import Text (plainText)
import List
import List (..)
import Outline.Notes.Model as Notes
import App.Command (..)

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
updateBlock action = updateZipper (Document.doBlock action)
updateSpan action = updateZipper (Document.doSpan action)

---- INPUT

stepFn : Command -> (Document.Zipper -> Document.Zipper)
stepFn c = case c of
  Key (Single Left) -> updateText Core.String.goLeft
  Key (Single Right) -> updateText Core.String.goRight
  Key (Single Down) -> updateEntry (Entry.doEntry EntryNav.goDownWithinChild)
  Key (Single Up) -> updateEntry (Entry.doEntry EntryNav.goUpWithinChild)
  Key (Single Enter) -> updateZipper Document.enter
  Key (CommandCharacter "a") -> updateEntry Entry.addInboxItem
  Key (CommandCharacter "d") -> updateText Core.String.delete
  Key (CommandCharacter "m") -> updateEntry Entry.missort
  Key (CommandCharacter "p") -> updateEntry Entry.promote
  Key (CommandCharacter "1") -> updateEntry (Entry.moveInto 0)
  Key (CommandCharacter "2") -> updateEntry (Entry.moveInto 1)
  Key (CommandCharacter "3") -> updateEntry (Entry.moveInto 2)
  Key (CommandCharacter "4") -> updateEntry (Entry.moveInto 3)
  Key (CommandCharacter "5") -> updateEntry (Entry.moveInto 4)
  Key (CommandCharacter "6") -> updateEntry (Entry.moveInto 5)
  Key (CommandCharacter "7") -> updateEntry (Entry.moveInto 6)
  Key (Alt Up) -> updateEntry (Entry.doEntry EntryNav.goToPrevSibling)
  Key (Alt Down) -> updateEntry (Entry.doEntry EntryNav.goToNextSibling)
  Key (Alt Right) -> updateEntry EntryNav.goToFirstChild
  Key (Alt Left) -> updateEntry EntryNav.goToParent
  Key (Command Up) -> updateEntry Entry.moveChildUp
  Key (Command Down) -> updateEntry Entry.moveChildDown
  Key (Command Right) -> updateText Core.String.moveToEndOfLine
  Key (Command Left) -> updateText Core.String.moveToStartOfLine

  -- Text
  Key (Single Backspace) -> updateZipper Document.backspace
  Key (Character s) -> updateText (Core.String.insert s)
  Paste s -> updateText (Core.String.insert s)

  -- Selection
  Key (Shift Left) -> updateText Core.String.selectLeft
  Key (Shift Right) -> updateText Core.String.selectRight
  Key (CommandShift Left) -> updateText Core.String.selectToStartOfLine
  Key (CommandShift Right) -> updateText Core.String.selectToEndOfLine

  -- Formatting
  Key (CommandCharacter "b") -> updateBlock (Block.toggleStyle Block.Task)

  Tab "Scratch" -> updateValue (Document.scratchZipper 0)
  Tab "Tasks" -> updateValue Document.outlineZipper
  Tab "Notes" -> updateValue Document.notesZipper

  Scratch i -> updateValue (Document.scratchZipper i)

  LoadedOutline (Ok e) -> Document.replaceOutline e
  LoadedScratch (Ok s) -> Document.replaceScratch s
  LoadedNotes (Ok n) -> Document.replaceNotes n

  ProcessScratch -> Document.processScratch
  NewScratch -> Document.newScratch

  x -> fst (identity, Debug.log "Unhandled command" x)

step : Command -> Document.Zipper -> Document.Zipper
step c m = stepFn c m
