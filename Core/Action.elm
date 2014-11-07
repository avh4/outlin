module Core.Action (Action, nav, change, always, Result(..)) where

data Result v c =
  Update (v,c) |
  Split [v] Int c |
  Delete |
  EnterPrev | EnterNext |
  NoChange

type Action v c = (v -> c -> Result v c)

nav : (v -> c -> c) -> Action v c
nav fn = \v c -> Update (v, (fn v c))

change : (v -> c -> v) -> Action v c
change fn = \v c -> Update ((fn v c), c)

always : Result v c -> Action v c
always r = \_ _ -> r
