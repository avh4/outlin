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

-- TODO: compare to try, tryMap in Outline.Entry
-- TODO: move to Util
tryMap : [(a -> Maybe b)] -> (a -> b) -> a -> b
tryMap fns fn v = case fns of
  [] -> fn v
  (head :: tail) -> case head v of
    Just result -> result
    Nothing -> tryMap tail fn v

-- TODO: could be written in terms of Maybe.map, and possibly inlined?
tryMap2 : [(a -> Maybe b)] -> (b -> x) -> x -> a -> x
tryMap2 fns fn2 def v = case fns of
  [] -> def
  (head :: tail) -> case head v of
    Just result -> fn2 result
    Nothing -> tryMap2 tail fn2 def v

goUpWithinChild : Zipper -> Result
goUpWithinChild z = case z of
  InText _ -> Action.NoChange
  InDescription _ -> toValue z |> textZipper |> Action.Update
  InInbox e -> case Core.Array.goPrev toValue textZipper e.inbox of
    Nothing -> toValue z |> tryMap [descriptionZipper] textZipper |> Action.Update
    Just result -> InInbox { e | inbox <- result } |> Action.Update
  _ -> Action.NoChange

goDownWithinChild : Zipper -> Result
goDownWithinChild z = case z of
  InText _ -> toValue z |> tryMap2 [descriptionZipper, firstInboxZipper] Action.Update Action.NoChange
  InDescription _ -> toValue z |> tryMap2 [firstInboxZipper] Action.Update Action.NoChange -- TODO rewrite without using tryMap
  InInbox e -> case Core.Array.goNext toValue textZipper e.inbox of
    Nothing -> Action.NoChange
    Just result -> Action.Update <| InInbox { e | inbox <- result }
  _ -> Action.NoChange

goToNextSibling : Zipper -> Result
goToNextSibling z = case z of
  InChild e -> case Core.Array.active e.children of
    InChild _ -> Action.NoChange
    _ -> case Core.Array.goNext toValue textZipper e.children of
      Just result -> Action.Update <| InChild { e | children <- result }
      Nothing -> Action.NoChange
  _ -> Action.NoChange

-- TODO: duplciates a lot of code with goToNextSibling...
goToPrevSibling : Zipper -> Result
goToPrevSibling z = case z of
  InChild e -> case Core.Array.active e.children of
    InChild _ -> Action.NoChange
    _ -> case Core.Array.goPrev toValue textZipper e.children of
      Just result -> Action.Update <| InChild { e | children <- result }
      Nothing -> Action.NoChange
  _ -> Action.NoChange
