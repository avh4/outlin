module App.EntryNav where

import Core.Action as Action
import Core.Array
import Outline.Entry (..)

goToFirstChild' : Zipper -> Result
goToFirstChild' z = case z of
  InChild _ -> Action.NoChange
  _ -> case toValue z of
    Entry e -> case Core.Array.firstZipperM textZipper e.children of
      Just result -> Action.Update <| InChild { e | children <- result }
      Nothing -> Action.NoChange

goToFirstChild : Zipper -> Result
goToFirstChild = doEntry goToFirstChild'

goToParent' : Zipper -> Result
goToParent' z = case z of
  InChild e -> case Core.Array.active e.children of
    InChild _ -> Action.NoChange
    _ -> Action.Update <| (toValue z |> textZipper)
  _ -> Action.NoChange

goToParent : Zipper -> Result
goToParent = doEntry goToParent'