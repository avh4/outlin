module Core.Action (Action, nav, change, always, Result(..), val, cur) where

data Result v c =
  Update v c |
  Split [v] Int c |
  Delete |
  NoChange

type Action v c = (v -> c -> Result v c)

nav : (v -> c -> c) -> Action v c
nav fn = \v c -> Update v (fn v c)

change : (v -> c -> v) -> Action v c
change fn = \v c -> Update (fn v c) c

always : Result v c -> Action v c
always r = \_ _ -> r

val : Result v c -> v
val r = case r of Update v _ -> v -- TODO: this function needs to go away

cur : Result v c -> c
cur r = case r of Update _ c -> c -- TODO: this function needs to go away
