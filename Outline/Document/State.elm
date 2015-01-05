module Outline.Document.State
  ( State(..)
  , toDocument
  , toScratch, toTasks, toNotes
  , doEntry, doText, doBlock, backspace
  , withDocument
  , selectScratch
  ) where

import Outline.Document.Model (..)
import Outline.Document.Actions (..)
import Outline.Document.Actions as Document
import Outline.Entry as Entry
import Outline.Scratch.Actions as Scratch
import Core.String
import RichText.BlockZipper (..)
import Outline.RichText.Block.Actions as Block

type State
  = InScratch Document
  | InTasks Document
  | InNotes Document

toDocument : State -> Document
toDocument state = case state of
  InScratch doc -> doc
  InTasks doc -> doc
  InNotes doc -> doc

to : (Document -> State) -> State -> State
to fn state = case state of
  InScratch doc -> fn doc
  InTasks doc -> fn doc
  InNotes doc -> fn doc

toScratch = to InScratch
toTasks = to InTasks
toNotes = to InNotes

ifScratch : (Document -> Document) -> State -> State
ifScratch fn state = case state of
  InScratch doc -> InScratch (fn doc)
  _ -> state

ifTasks : (Document -> Document) -> State -> State
ifTasks fn state = case state of
  InTasks doc -> InTasks (fn doc)
  _ -> state

doEntry : (Entry.Zipper -> Entry.Result) -> State -> State
doEntry entryFn state = state
  |> ifTasks (doTasksAction entryFn)

doText : (Core.String.Zipper -> Core.String.Result) -> State -> State
doText fn state = state
  |> ifScratch (doScratchAction (Scratch.doText fn))
  |> ifTasks (doTasksAction (Entry.do fn))

doBlock : (BlockZipper -> Block.Result) -> State -> State
doBlock fn state = state
  |> ifScratch (doScratchAction (Scratch.doBlock fn))

withDocument : (Document -> Document) -> State -> State
withDocument fn state = case state of
  InScratch doc -> InScratch (fn doc)
  InTasks doc -> InTasks (fn doc)
  InNotes doc -> InNotes (fn doc)

backspace : State -> State
backspace state = state
  |> ifScratch (doScratchAction (Scratch.doBlock Block.backspace))
  |> ifTasks (doTasksAction (Entry.do Core.String.backspace))

selectScratch : Int -> State -> State
selectScratch i state = state
  |> toScratch
  |> ifScratch (Document.selectScratch i)
