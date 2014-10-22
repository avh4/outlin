module Core.Array where

type Cursor a = (Int, a)

render : (val -> Maybe cur -> out) -> [val] -> Maybe (Cursor cur) -> [out]
render fn list msel = case msel of
  Just (n, c) -> indexedMap (\i x -> fn x (if i==n then Just c else Nothing)) list
  Nothing -> map (\x -> fn x Nothing) list
