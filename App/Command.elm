module App.Command
  ( Command(..)
  ) where

import Outline.Entry as Entry
import Outline.Scratch.Model as Scratch
import Outline.Notes.Model as Notes
import Keys

type Command
  = Init_
  | Key Keys.KeyCombo
  | Paste String
  | LoadedOutline (Result String Entry.Value)
  | LoadedScratch (Result String (List Scratch.Value))
  | LoadedNotes (Result String Notes.Value)
  | Tab String
  | Scratch Int
  | ProcessScratch
  | NewScratch
