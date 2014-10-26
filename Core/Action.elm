module Core.Action (Action, split, nav, change, apply) where

-- data Result v c =
--   Update v c

type Result v c = (v, c)

type Action v c = (v -> c -> Result v c)

split : (v -> c -> Result v c) -> Action v c
split fn = fn

nav : (v -> c -> c) -> Action v c
nav fn = \v c -> (v, fn v c)

change : (v -> c -> v) -> Action v c
change fn = \v c -> (fn v c, c)

apply : Action v c -> v -> c -> Result v c
apply action = action
