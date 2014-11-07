module Core.Action (Action, nav, change, always, Result(..)) where

data Result value zipper =
  Update zipper |
  Split [value] zipper [value] |
  Delete |
  EnterPrev | EnterNext |
  NoChange

type Action v c = (v -> c -> Result v (v,c))

nav : (v -> c -> c) -> Action v c
nav fn = \v c -> Update (v, (fn v c))

change : (v -> c -> v) -> Action v c
change fn = \v c -> Update ((fn v c), c)

always : Result v (v,c) -> Action v c
always r = \_ _ -> r
