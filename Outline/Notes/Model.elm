module Outline.Notes.Model
  ( Value
  ) where

import Core.Array
import Outline.RichText.Model as RichText

type alias Value = Core.Array.Value RichText.Value
