module Outline.Document.Actions
  ( Result
  , do, doEntry, doBlock, doSpan, doText
  , enter, backspace, processScratch
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
    EnterNext -> Update <| (zipper |> toValue |> outlineZipper)
    NoChange -> NoChange
  InOutline r -> case entryFn r.outline of
    Update eZipper' -> Update <| InOutline { r | outline <- eZipper' }
    Split _ _ _ -> NoChange -- Not allowed to split the root node
    Delete -> NoChange -- Not allowed to delete the root node
    EnterPrev -> case Core.Array.firstZipperM Scratch.endZipper r.scratch of
      Just sZipper -> Update <| InScratch { r
        | scratch <- sZipper
        , outline <- Entry.toValue r.outline
        }
      Nothing -> NoChange -- TODO: make a new empty scratch
    EnterNext -> NoChange
    NoChange -> NoChange

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

replaceOutline : Entry.Value -> Zipper -> Zipper
replaceOutline outline' z = case z of
  InScratch r -> InScratch { r | outline <- outline' }
  InOutline r -> InOutline { r | outline <- outline' |> Entry.textZipper }

addNote : RichText.Value -> Zipper -> Zipper
addNote note z = case z of
  InScratch r -> InScratch { r | notes <- note :: r.notes }
  InOutline r -> InOutline { r | notes <- note :: r.notes }

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
          |> replaceOutline (Entry.addToInbox newTasks r.outline)
          |> addNote scratchValue
        _ -> InOutline { r
          | scratch <- []
          , outline <- (Entry.addToInbox newTasks r.outline |> Entry.textZipper)
          }
          |> addNote scratchValue
  _ -> m
