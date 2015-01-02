module Outline.Document.Model
  ( Value, Zipper(..)
  , emptyValue
  , toValue
  , scratchZipper, tasksZipper, notesZipper
  , replaceTasks, replaceScratch, replaceNotes
  ) where

import Core.Action
import Core.Action (..)
import Core.Array
import Outline.Entry as Entry
import Outline.Scratch.Model as Scratch
import Outline.RichText.Model as RichText
import Outline.Notes.Model as Notes


type alias Value =
  { scratch:Core.Array.Value Scratch.Value
  , tasks:Entry.Value
  , notes:Core.Array.Value RichText.Value
  }
type Zipper
  = InScratch
      { scratch:(Core.Array.Zipper Scratch.Value Scratch.Zipper)
      , tasks:Entry.Value
      , notes:(Core.Array.Value RichText.Value)
      }
  | InTasks
      { scratch:(Core.Array.Value Scratch.Value)
      , tasks:Entry.Zipper
      , notes:(Core.Array.Value RichText.Value)
      }
  | InNotesArchive
      { scratch:Core.Array.Value Scratch.Value
      , tasks:Entry.Value
      , notes:Core.Array.Value RichText.Value
      }

emptyValue : Value
emptyValue =
  { scratch=[]
  , tasks=Entry.emptyEntry
  , notes=[]
  }

toValue : Zipper -> Value
toValue z = case z of
  InScratch r -> { r | scratch <- r.scratch |> Core.Array.toValue Scratch.toValue }
  InTasks r -> { r | tasks <- r.tasks |> Entry.toValue }
  InNotesArchive r -> r

scratchZipper : Int -> Value -> Zipper
scratchZipper i r = case Core.Array.zipperAtM i Scratch.endZipper r.scratch of
  Just zipper -> InScratch { r | scratch <- zipper }
  Nothing -> InScratch { r | scratch <- ([Scratch.value "Scratch 1"] |> Core.Array.firstZipper Scratch.allZipper) }

tasksZipper : Value -> Zipper
tasksZipper r = InTasks { r | tasks <- r.tasks |> Entry.textZipper }

notesZipper : Value -> Zipper
notesZipper r = InNotesArchive r

replaceTasks : Entry.Value -> Zipper -> Zipper
replaceTasks tasks' z = case z of
  InScratch r -> InScratch { r | tasks <- tasks' }
  InTasks r -> InTasks { r | tasks <- tasks' |> Entry.textZipper }
  InNotesArchive r -> InNotesArchive { r | tasks <- tasks' }

replaceScratch : Core.Array.Value Scratch.Value -> Zipper -> Zipper
replaceScratch scratch' z = case z of
  InScratch r -> case Core.Array.firstZipperM Scratch.endZipper scratch' of
    Just scratch'' -> InScratch { r | scratch <- scratch'' }
    Nothing -> z -- TODO: should create an empty scratch
  InTasks r -> InTasks { r | scratch <- scratch' }
  InNotesArchive r -> InNotesArchive { r | scratch <- scratch' }

replaceNotes : Notes.Value -> Zipper -> Zipper
replaceNotes notes' z = case z of
  InScratch r -> InScratch { r | notes <- notes' }
  InTasks r -> InTasks { r | notes <- notes' }
  InNotesArchive r -> InNotesArchive { r | notes <- notes' }
