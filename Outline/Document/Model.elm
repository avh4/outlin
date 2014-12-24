module Outline.Document.Model
  ( Value, Zipper(..)
  , emptyValue
  , toValue
  , scratchZipper, outlineZipper, notesZipper
  , replaceOutline, replaceScratch, replaceNotes
  ) where

import Core.Action
import Core.Action (..)
import Core.Array
import Outline.Entry as Entry
import Outline.Scratch.Model as Scratch
import Outline.RichText.Model as RichText
import Outline.Notes.Model as Notes


-- TODO: rename outline -> tasks?
type alias Value =
  { scratch:Core.Array.Value Scratch.Value
  , outline:Entry.Value
  , notes:Core.Array.Value RichText.Value
  }
type Zipper
  = InScratch
      { scratch:(Core.Array.Zipper Scratch.Value Scratch.Zipper)
      , outline:Entry.Value
      , notes:(Core.Array.Value RichText.Value)
      }
  | InOutline
      { scratch:(Core.Array.Value Scratch.Value)
      , outline:Entry.Zipper
      , notes:(Core.Array.Value RichText.Value)
      }
  | InNotesArchive
      { scratch:Core.Array.Value Scratch.Value
      , outline:Entry.Value
      , notes:Core.Array.Value RichText.Value
      }

emptyValue : Value
emptyValue =
  { scratch=[]
  , outline=Entry.emptyEntry
  , notes=[]
  }

toValue : Zipper -> Value
toValue z = case z of
  InScratch r -> { r | scratch <- r.scratch |> Core.Array.toValue Scratch.toValue }
  InOutline r -> { r | outline <- r.outline |> Entry.toValue }
  InNotesArchive r -> r

scratchZipper : Int -> Value -> Zipper
scratchZipper i r = case Core.Array.zipperAtM i Scratch.endZipper r.scratch of
  Just zipper -> InScratch { r | scratch <- zipper }
  Nothing -> InScratch { r | scratch <- ([Scratch.value "Scratch 1"] |> Core.Array.firstZipper Scratch.allZipper) }

outlineZipper : Value -> Zipper
outlineZipper r = InOutline { r | outline <- r.outline |> Entry.textZipper }

notesZipper : Value -> Zipper
notesZipper r = InNotesArchive r

replaceOutline : Entry.Value -> Zipper -> Zipper
replaceOutline outline' z = case z of
  InScratch r -> InScratch { r | outline <- outline' }
  InOutline r -> InOutline { r | outline <- outline' |> Entry.textZipper }
  InNotesArchive r -> InNotesArchive { r | outline <- outline' }

replaceScratch : Core.Array.Value Scratch.Value -> Zipper -> Zipper
replaceScratch scratch' z = case z of
  InScratch r -> case Core.Array.firstZipperM Scratch.endZipper scratch' of
    Just scratch'' -> InScratch { r | scratch <- scratch'' }
    Nothing -> z -- TODO: should create an empty scratch
  InOutline r -> InOutline { r | scratch <- scratch' }
  InNotesArchive r -> InNotesArchive { r | scratch <- scratch' }

replaceNotes : Notes.Value -> Zipper -> Zipper
replaceNotes notes' z = case z of
  InScratch r -> InScratch { r | notes <- notes' }
  InOutline r -> InOutline { r | notes <- notes' }
  InNotesArchive r -> InNotesArchive { r | notes <- notes' }
