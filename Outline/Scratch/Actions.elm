module Outline.Scratch.Actions (Result, do) where

import Outline.Scratch.Model (..)
import Core.Action
import Core.Array
import Core.String

type alias Result = Core.Action.Result Value Zipper

do : (Core.String.Zipper -> Core.String.Result) -> Zipper -> Result
do stringFn = stringFn
