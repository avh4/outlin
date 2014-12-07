module Outline.Document.Model (Value, Zipper(..), toValue, scratchZipper, outlineZipper) where

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

toValue : Zipper -> Value
toValue z = case z of
  InScratch sZip eVal -> {scratch=Core.Array.toValue Scratch.toValue sZip, outline=eVal}
  InOutline sVal eZip -> {scratch=sVal, outline=Entry.toValue eZip}

scratchZipper : Int -> Value -> Zipper
scratchZipper i {scratch,outline} = case Core.Array.zipperAtM i Scratch.endZipper scratch of
  Just zipper -> InScratch zipper outline
  -- Nothing ->  TODO-- should create an empty scratch

outlineZipper : Value -> Zipper
outlineZipper {scratch,outline} = InOutline scratch (Entry.textZipper outline)
