module Core.Action (Action, split, nav, apply) where

type Action val cur = {
  valueFn:(val -> cur -> val),
  curFn:(val -> cur -> cur)
  }

split : (v -> c -> (v, c)) -> Action v c
split fn = Action (\v c -> fst <| fn v c) (\v c -> snd <| fn v c)

nav : (v -> c -> c) -> Action v c
nav fn = Action (\v _ -> v) fn

apply : Action val cur -> val -> cur -> (val, cur)
apply action v c = (action.valueFn v c, action.curFn v c)
