module Outline.Document.Actions
  ( processScratch, newScratch, selectScratch
  , doTasksAction, doScratchAction
  ) where

import Outline.Document.Model (..)
import Core.Action (..)
import Core.Array
import Core.String
import Outline.Entry as Entry
import Outline.Scratch.Model as Scratch
import Outline.Scratch.Actions as Scratch
import Outline.RichText.Model as RichText
import Outline.RichText.Model
import Outline.RichText.Span.Actions as Span
import Outline.RichText.Block.Actions as Block
import List
import List ((::))
import RichText
import RichText.SpanZipper as RichText
import RichText.BlockZipper as RichText

doScratchAction : (Scratch.Zipper -> Scratch.Result) -> Document -> Document
doScratchAction scratchFn doc = case Core.Array.do Scratch.toValue Scratch.endZipper Scratch.endZipper scratchFn doc.scratch of
  Update scratch' -> { doc | scratch <- scratch' }
  Delete -> { doc | scratch <- [[RichText.heading "Scratch 1"]] |> Core.Array.firstZipper Scratch.allZipper }
  _ -> doc

doTasksAction : (Entry.Zipper -> Entry.Result) -> Document -> Document
doTasksAction entryFn doc = case entryFn doc.tasks of
  Update tasks' -> { doc | tasks <- tasks' }
  _ -> doc

doTasks : (Entry.Zipper -> Entry.Zipper) -> Document -> Document
doTasks fn doc = { doc | tasks <- fn doc.tasks }

doTasksValue : (Entry.Value -> Entry.Value) -> Document -> Document
doTasksValue fn = doTasks (Entry.toValue >> fn >> Entry.textZipper)

doNotes : (List RichText.Value -> List RichText.Value) -> Document -> Document
doNotes fn doc = { doc | notes <- fn doc.notes }

selectScratch : Int -> Document -> Document
selectScratch i doc = case Core.Array.zipperAtM i Scratch.endZipper (doc.scratch |> Core.Array.toValue Scratch.toValue) of
  Just scratch' -> { doc | scratch <- scratch' }
  Nothing -> doc

processScratch : Document -> Document
processScratch doc =
  let
    currentScratch = Core.Array.active doc.scratch
    scratchValue = currentScratch |> Scratch.toValue
    newTasks = scratchValue |> RichText.getTasks |> List.map RichText.toPlainText
  in
    doc
      |> doScratchAction (\_ -> Delete)
      |> doTasks (Entry.addToInbox newTasks)
      |> doNotes (\ns -> scratchValue :: ns)

newScratch : Document -> Document
newScratch doc =
  let
    scratch = Core.Array.toValue Outline.RichText.Model.toValue doc.scratch
    title = ("Scratch " ++ toString (1 + List.length scratch))
    scratch' = [RichText.heading title] :: scratch
  in
    { doc | scratch <- scratch' |> Core.Array.firstZipper Scratch.allZipper }
