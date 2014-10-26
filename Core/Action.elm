module Core.Action (Action, nav, change) where

-- data Result v c =
--   Update v c

type Result v c = (v, c)

type Action v c = (v -> c -> Result v c)

nav : (v -> c -> c) -> Action v c
nav fn = \v c -> (v, fn v c)

change : (v -> c -> v) -> Action v c
change fn = \v c -> (fn v c, c)
