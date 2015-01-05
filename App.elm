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
import Outline.RichText.Span.Actions as Span
import Outline.RichText.Block.Actions as Block
import App.EntryNav as EntryNav
import Graphics.Element (flow, right, down, Element, width, heightOf, widthOf, spacer, color, container, topLeft, midLeft)
import Graphics.Collage (collage, toForm, rotate)
import Text (plainText)
import List
import List (..)
import Outline.Notes.Model as Notes
import App.Command (..)
import RichText
import Outline.Document.State as State
import Outline.Document.State (State)

---- App

justUpdate : (z -> z) -> (z -> Action.ActionResult v z)
justUpdate fn z = Action.Update <| fn z

updateEntry action = State.doEntry action
updateText action = State.doText action
updateTextZipper action = State.doText (justUpdate action)
updateBlock action = State.doBlock action
updateSpan action = State.doBlock (Block.doSpan action)

---- INPUT

step : Command -> State -> State
step c = case c of
  Key (Single Left) -> updateText Core.String.goLeft
  Key (Single Right) -> updateText Core.String.goRight
  Key (Single Down) -> updateEntry (Entry.doEntry EntryNav.goDownWithinChild)
  Key (Single Up) -> updateEntry (Entry.doEntry EntryNav.goUpWithinChild)
  Key (Single Enter) -> updateText Core.String.split
  Key (CommandCharacter "a") -> updateEntry Entry.addInboxItem
  Key (CommandCharacter "d") -> updateText (\_ -> Action.Delete)
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
  Key (Command Right) -> updateTextZipper Core.String.moveToEndOfLine
  Key (Command Left) -> updateTextZipper Core.String.moveToStartOfLine

  -- Text
  Key (Single Backspace) -> State.backspace
  Key (Character s) -> updateTextZipper (Core.String.insert s)
  Paste s -> updateTextZipper (Core.String.insert s)

  -- Selection
  Key (Shift Left) -> updateTextZipper Core.String.selectLeft
  Key (Shift Right) -> updateTextZipper Core.String.selectRight
  Key (CommandShift Left) -> updateTextZipper Core.String.selectToStartOfLine
  Key (CommandShift Right) -> updateTextZipper Core.String.selectToEndOfLine

  -- Formatting
  Key (CommandCharacter "b") -> updateBlock (Block.toggleStyle RichText.Task >> Block.Update)

  Tab "Scratch" -> State.toScratch
  Tab "Tasks" -> State.toTasks
  Tab "Notes" -> State.toNotes

  Scratch i -> State.selectScratch i

  LoadedTasks (Ok e) -> State.withDocument <| Document.replaceTasks e
  LoadedScratch (Ok s) -> State.withDocument <| Document.replaceScratch s
  LoadedNotes (Ok n) -> State.withDocument <| Document.replaceNotes n

  ProcessScratch -> State.withDocument <| Document.processScratch
  NewScratch -> State.withDocument <| Document.newScratch

  x -> fst (identity, Debug.log "Unhandled command" x)
