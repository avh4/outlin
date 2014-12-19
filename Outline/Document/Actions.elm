module Outline.Document.Actions
  ( Result
  , do, doEntry, doBlock, doSpan, doText
  , enter, processScratch
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

type alias Result = ActionResult Value Zipper

do : (Scratch.Zipper -> Scratch.Result) -> (Entry.Zipper -> Entry.Result) -> Zipper -> Result
do scratchFn entryFn zipper = case zipper of
  InScratch sZipper eVal -> case Core.Array.do Scratch.toValue Scratch.endZipper Scratch.endZipper scratchFn sZipper of
    Update sZipper' -> Update <| InScratch sZipper' eVal
    Split _ _ _ -> NoChange -- Core.Array can't even propogate a split, right?
    Delete -> NoChange -- TODO: replace scratches with new empty scratch
    EnterPrev -> NoChange
    EnterNext -> Update <| InOutline (Core.Array.toValue Scratch.toValue sZipper) (Entry.textZipper eVal)
    NoChange -> NoChange
  InOutline sVal eZipper -> case entryFn eZipper of
    Update eZipper' -> Update <| InOutline sVal eZipper'
    Split _ _ _ -> NoChange -- Not allowed to split the root node
    Delete -> NoChange -- Not allowed to delete the root node
    EnterPrev -> case Core.Array.firstZipperM Scratch.endZipper sVal of
      Just sZipper -> Update <| InScratch sZipper (Entry.toValue eZipper)
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

replaceTasks : Entry.Value -> Zipper -> Zipper
replaceTasks newTasks z = case z of
  InScratch sz tv -> InScratch sz newTasks
  InOutline sv tz -> InOutline sv (newTasks |> Entry.textZipper)

processScratch : Zipper -> Zipper
processScratch m = case m of
  InScratch z ev ->
    let
      currentScratch = Core.Array.active z
      newTasks = currentScratch |> Scratch.toValue |> RichText.getTasks |> List.map Block.toString
    in
      case m |> doScratch (\_ -> Delete) of
        Update m' -> m' |> replaceTasks (Entry.addToInbox newTasks ev)
        _ -> InOutline [] (Entry.addToInbox newTasks ev |> Entry.textZipper)
  _ -> m
