module Outline.Notes.Model
  ( Value
  ) where

import Core.Array
import Outline.RichText.Model as RichText

type alias Value = List RichText.Value
