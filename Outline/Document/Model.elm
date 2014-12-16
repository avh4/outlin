module Outline.Document.Model
  ( Value, Zipper(..)
  , emptyValue
  , toValue, scratchValue, outlineValue
  , scratchZipper, outlineZipper
  , replaceOutline, replaceScratch
  ) where

import Core.Action
import Core.Action (..)
import Core.Array
import Outline.Entry as Entry
import Outline.Scratch.Model as Scratch

type alias Value =
  { scratch:Core.Array.Value Scratch.Value
  , outline:Entry.Value
  }
type Zipper
  = InScratch (Core.Array.Zipper Scratch.Value Scratch.Zipper) Entry.Value
  | InOutline (Core.Array.Value Scratch.Value) Entry.Zipper

emptyValue : Value
emptyValue =
  { scratch=[]
  , outline=Entry.emptyEntry
  }

toValue : Zipper -> Value
toValue z = case z of
  InScratch sZip eVal -> {scratch=Core.Array.toValue Scratch.toValue sZip, outline=eVal}
  InOutline sVal eZip -> {scratch=sVal, outline=Entry.toValue eZip}

outlineValue : Zipper -> Entry.Value
outlineValue z = z |> toValue |> .outline

scratchValue : Zipper -> Core.Array.Value Scratch.Value
scratchValue z = z |> toValue |> .scratch

scratchZipper : Int -> Value -> Zipper
scratchZipper i {scratch,outline} = case Core.Array.zipperAtM i Scratch.endZipper scratch of
  Just zipper -> InScratch zipper outline
  Nothing -> InScratch ([Scratch.value "Scratch 1"] |> Core.Array.firstZipper Scratch.allZipper) outline

outlineZipper : Value -> Zipper
outlineZipper {scratch,outline} = InOutline scratch (Entry.textZipper outline)

replaceOutline : Zipper -> Entry.Value -> Zipper
replaceOutline z outline = case z of
  InScratch sZip _ -> InScratch sZip outline
  InOutline sVal _ -> InOutline sVal (Entry.textZipper outline)

replaceScratch : Zipper -> Core.Array.Value Scratch.Value -> Zipper
replaceScratch z scratch = case z of
  InScratch _ eVal -> case Core.Array.firstZipperM Scratch.endZipper scratch of
    Just sZip -> InScratch sZip eVal
    Nothing -> z -- TODO: should create an empty scratch
  InOutline _ eZip -> InOutline scratch eZip
