module Core.Action (always, Result(..)) where

type Result value zipper =
  Update zipper |
  Split (List value) zipper (List value) |
  Delete |
  EnterPrev | EnterNext |
  NoChange

-- TODO: get rid of this--only used in tests
always : Result v z -> z -> Result v z
always r _ = r
