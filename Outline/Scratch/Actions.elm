module Outline.Scratch.Actions where

import Outline.Scratch.Model (..)
import Core.Action
import Core.Array
import Core.String

type alias Result = Core.Action.Result Value Zipper

goLeft = Core.String.goLeft
