module Core.Action (ActionResult(..)) where

type ActionResult value zipper =
  Update zipper |
  Split (List value) zipper (List value) |
  Delete |
  EnterPrev | EnterNext |
  NoChange
