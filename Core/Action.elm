module Core.Action (Action, split, nav) where

type Action val cur = {
  valueFn:(val -> cur -> val),
  curFn:(val -> cur -> cur)
  }

split : (v -> c -> (v, c)) -> Action v c
split fn = Action (\v c -> fst <| fn v c) (\v c -> snd <| fn v c)

nav : (v -> c -> c) -> Action v c
nav fn = Action (\v _ -> v) fn
