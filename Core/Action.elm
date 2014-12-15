module Core.Action (always, ActionResult(..)) where

type ActionResult value zipper =
  Update zipper |
  Split (List value) zipper (List value) |
  Delete |
  EnterPrev | EnterNext |
  NoChange

-- TODO: get rid of this--only used in tests
always : ActionResult v z -> z -> ActionResult v z
always r _ = r
