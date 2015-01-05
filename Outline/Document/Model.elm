module Outline.Document.Model
  ( Document
  , emptyValue
  , replaceTasks, replaceScratch, replaceNotes
  ) where

import Core.Array
import Outline.Entry as Entry
import Outline.Scratch.Model as Scratch
import Outline.RichText.Model as RichText
import Outline.Notes.Model as Notes

type alias Document =
  { scratch:(Core.Array.Zipper Scratch.Value Scratch.Zipper)
  , tasks:Entry.Zipper
  , notes:List RichText.Value
  }

emptyValue : Document
emptyValue =
  { scratch=[Scratch.value "Scratch 1"] |> Core.Array.firstZipper Scratch.allZipper
  , tasks=Entry.emptyEntry |> Entry.textZipper
  , notes=[]
  }

replaceTasks : Entry.Value -> Document -> Document
replaceTasks tasks' d = { d | tasks <- tasks' |> Entry.textZipper }

replaceScratch : List Scratch.Value -> Document -> Document
replaceScratch scratch' d = case Core.Array.firstZipperOr (Scratch.value "Scratch 1") Scratch.endZipper scratch' of
  scratch'' -> { d | scratch <- scratch'' }

replaceNotes : Notes.Value -> Document -> Document
replaceNotes notes' d = { d | notes <- notes' }
