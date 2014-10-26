module Core.Array (Cursor, cursor, do, split, render, toJson) where

import Core.Action (Action)
import Core.Action as Action

type Value a = [a]
type Cursor a = (Int, a)
type Subs a = {child:a}

cursor n c = (n, c)

replaceAt : a -> Int -> [a] -> [a]
replaceAt a index list =
  indexedMap (\i item -> if i == index then a else item) list

at : Int -> [a] -> a
at i list = list |> drop i |> head

do : c -> Action v c -> Action [v] (Cursor c)
do nextCursor action vs (i,c) = case action (at i vs) c of
  Action.Update newV newC -> Action.Update (replaceAt newV i vs) (i,newC)
  Action.Split newVs newI newC -> Action.Update ((take i vs) ++ newVs ++ (drop (i+1) vs)) (newI+i, newC)
  Action.Delete -> if
    | length vs > 1 -> Action.Update ((take i vs) ++ (drop (i+1) vs)) (min i <| -2+length vs,c)
    | otherwise -> Action.Delete
  Action.EnterNext -> if
    | length vs > i+1 -> Action.Update vs (i+1, nextCursor)
    | otherwise -> Action.EnterNext
  Action.EnterPrev -> if
    | i > 0 -> Action.Update vs (i-1, c)
    | otherwise -> Action.EnterPrev
  Action.NoChange -> Action.NoChange

split_ : (v -> c -> (v, v, c)) -> Action v c
split_ fn = \v c -> case fn v c of
  (v1, v2, innerC) -> Action.Split [v1, v2] 1 innerC

split : c -> (v -> c -> (v, v, c)) -> Action [v] (Cursor c)
split nextCursor fn = do nextCursor (split_ fn)

render : (val -> Maybe cur -> out) -> [val] -> Maybe (Cursor cur) -> [out]
render fn list msel = case msel of
  Just (n, c) -> indexedMap (\i x -> fn x (if i==n then Just c else Nothing)) list
  Nothing -> map (\x -> fn x Nothing) list


---- JSON

walk : (Value b -> c) -> Subs (a -> b) -> Value a -> c
walk wrapFn {child} list = wrapFn <| map child list

toJson : (a -> String) -> [a] -> String
toJson fn = walk (\vs -> "[" ++ (join "," vs) ++ "]") {child=fn}
