module Core.Action (nav, change, always, Result(..)) where

type Result value zipper =
  Update zipper |
  Split (List value) zipper (List value) |
  Delete |
  EnterPrev | EnterNext |
  NoChange

nav : (v -> c -> c) -> (v,c) -> Result v (v,c)
nav fn = \(v,c) -> Update (v, (fn v c))

change : (v -> c -> v) -> (v,c) -> Result v (v,c)
change fn = \(v,c) -> Update ((fn v c), c)

always : Result v (v,c) -> (v,c) -> Result v (v,c)
always r = \_ -> r
