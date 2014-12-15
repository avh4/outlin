module Outline.Document.Actions (Result, do, doEntry, doText, enter) where

import Outline.Document.Model (..)
import Core.Action (..)
import Core.Array
import Core.String
import Outline.Entry as Entry
import Outline.Scratch.Model as Scratch
import Outline.Scratch.Actions as Scratch

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

doEntry : (Entry.Zipper -> Entry.Result) -> Zipper -> Result
doEntry entryFn = do (\_ -> NoChange) entryFn

doText : (Core.String.Zipper -> Core.String.Result) -> Zipper -> Result
doText stringFn = do
  (Scratch.doText stringFn)
  (Entry.do stringFn)

enter : Zipper -> Result
enter = do
  (Scratch.doText (Core.String.insert "\n"))
  (Entry.do Core.String.split)
