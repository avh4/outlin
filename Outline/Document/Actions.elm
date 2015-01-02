module Outline.Document.Actions
  ( Result
  , do, doEntry, doBlock, doSpan, doText
  , enter, backspace, processScratch, newScratch
  ) where

import Outline.Document.Model (..)
import Core.Action (..)
import Core.Array
import Core.String
import Outline.Entry as Entry
import Outline.Scratch.Model as Scratch
import Outline.Scratch.Actions as Scratch
import Outline.RichText.Model as RichText
import Outline.RichText.Span.Model as Span
import Outline.RichText.Span.Actions as Span
import Outline.RichText.Block.Model as Block
import Outline.RichText.Block.Actions as Block
import List
import List ((::))

type alias Result = ActionResult Value Zipper

do : (Scratch.Zipper -> Scratch.Result) -> (Entry.Zipper -> Entry.Result) -> Zipper -> Result
do scratchFn entryFn zipper = case zipper of
  InScratch r -> case Core.Array.do Scratch.toValue Scratch.endZipper Scratch.endZipper scratchFn r.scratch of
    Update sZipper' -> Update <| InScratch { r | scratch <- sZipper' }
    Split _ _ _ -> NoChange -- Core.Array.do can't even propogate a split, right?
    Delete -> NoChange -- TODO: replace scratches with new empty scratch
    EnterPrev -> NoChange
    EnterNext -> Update <| (zipper |> toValue |> tasksZipper)
    NoChange -> NoChange
  InTasks r -> case entryFn r.tasks of
    Update eZipper' -> Update <| InTasks { r | tasks <- eZipper' }
    Split _ _ _ -> NoChange -- Not allowed to split the root node
    Delete -> NoChange -- Not allowed to delete the root node
    EnterPrev -> case Core.Array.firstZipperM Scratch.endZipper r.scratch of
      Just sZipper -> Update <| InScratch { r
        | scratch <- sZipper
        , tasks <- Entry.toValue r.tasks
        }
      Nothing -> NoChange -- TODO: make a new empty scratch
    EnterNext -> NoChange
    NoChange -> NoChange
  InNotesArchive r -> NoChange

doScratch : (Scratch.Zipper -> Scratch.Result) -> Zipper -> Result
doScratch scratchFn = do scratchFn (\_ -> NoChange)

doEntry : (Entry.Zipper -> Entry.Result) -> Zipper -> Result
doEntry entryFn = do (\_ -> NoChange) entryFn

doBlock : (Block.Zipper -> Block.Result) -> Zipper -> Result
doBlock blockFn = do
  (Scratch.doBlock blockFn)
  (\_ -> NoChange)

doSpan : (Span.Zipper -> Span.Result) -> Zipper -> Result
doSpan spanFn = do
  (Scratch.doSpan spanFn)
  (\_ -> NoChange)

doText : (Core.String.Zipper -> Core.String.Result) -> Zipper -> Result
doText stringFn = do
  (Scratch.doText stringFn)
  (Entry.do stringFn)

enter : Zipper -> Result
enter = doText Core.String.split

backspace : Zipper -> Result
backspace = do
  (Scratch.doBlock Block.backspace)
  (Entry.do Core.String.backspace)

replaceTasks : Entry.Value -> Zipper -> Zipper
replaceTasks tasks' z = case z of
  InScratch r -> InScratch { r | tasks <- tasks' }
  InTasks r -> InTasks { r | tasks <- tasks' |> Entry.textZipper }
  InNotesArchive r -> InNotesArchive { r | tasks <- tasks' }

addNote : RichText.Value -> Zipper -> Zipper
addNote note z = case z of
  InScratch r -> InScratch { r | notes <- note :: r.notes }
  InTasks r -> InTasks { r | notes <- note :: r.notes }
  InNotesArchive r -> InNotesArchive { r | notes <- note :: r.notes }

processScratch : Zipper -> Zipper
processScratch m = case m of
  InScratch r ->
    let
      currentScratch = Core.Array.active r.scratch
      scratchValue = currentScratch |> Scratch.toValue
      newTasks = scratchValue |> RichText.getTasks |> List.map Block.toString
    in
      case m |> doScratch (\_ -> Delete) of
        Update m' -> m'
          |> replaceTasks (Entry.addToInbox newTasks r.tasks)
          |> addNote scratchValue
        _ -> InTasks { r
          | scratch <- []
          , tasks <- (Entry.addToInbox newTasks r.tasks |> Entry.textZipper)
          }
          |> addNote scratchValue
  _ -> m

addScratch : Value -> Value
addScratch r = case ("Scratch " ++ toString (1 + List.length r.scratch)) of
  title -> { r | scratch <- (RichText.heading title) :: r.scratch }

newScratch : Zipper -> Zipper
newScratch z = case z |> toValue of
  v -> v
    |> addScratch
    |> scratchZipper 0
