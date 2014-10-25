module Core.Array (Cursor, applyAt, split, render, toJson) where

import Core.Action (Action)
import Core.Action as Action

type Value a = [a]
type Cursor a = (Int, a)
type Subs a = {child:a}

changeAt : (a -> a) -> Int -> [a] -> [a]
changeAt fn index list =
  indexedMap (\i item -> if i == index then fn item else item) list

at : Int -> [a] -> a
at i list = list |> drop i |> head

applyAt : Action v a -> Action [v] (Cursor a)
applyAt {valueFn,curFn} = Action
  (\vs (i,c) -> changeAt (\x -> valueFn x c) i vs)
  (\vs (i,c) -> (i, curFn (at i vs) c))

do : (v -> c -> ([v], Cursor c)) -> Action [v] (Cursor c)
do fn = Action.split (\vs (i,c) -> fn (at i vs) c)

split_ : (v -> c -> (v, v, c)) -> (v -> c -> ([v], Cursor c))
split_ fn = \v c -> case fn v c of
  (v1, v2, innerC) -> ([v1, v2], (1, innerC))

split : (v -> c -> (v, v, c)) -> Action [v] (Cursor c)
split fn = do (split_ fn)

render : (val -> Maybe cur -> out) -> [val] -> Maybe (Cursor cur) -> [out]
render fn list msel = case msel of
  Just (n, c) -> indexedMap (\i x -> fn x (if i==n then Just c else Nothing)) list
  Nothing -> map (\x -> fn x Nothing) list


---- JSON

walk : (Value b -> c) -> Subs (a -> b) -> Value a -> c
walk wrapFn {child} list = wrapFn <| map child list

toJson : (a -> String) -> [a] -> String
toJson fn = walk (\vs -> "[" ++ (join "," vs) ++ "]") {child=fn}
