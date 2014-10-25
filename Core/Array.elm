module Core.Array where

import Core.Action (Action)

type Cursor a = (Int, a)

changeAt : (a -> a) -> Int -> [a] -> [a]
changeAt fn index list =
  indexedMap (\i item -> if i == index then fn item else item) list

at : Int -> [a] -> a
at i list = list |> drop i |> head

applyAt : Action v a -> Action [v] (Cursor a)
applyAt {valueFn,curFn} = Action
  (\vs (i,c) -> changeAt (\x -> valueFn x c) i vs)
  (\vs (i,c) -> (i, curFn (at i vs) c))

render : (val -> Maybe cur -> out) -> [val] -> Maybe (Cursor cur) -> [out]
render fn list msel = case msel of
  Just (n, c) -> indexedMap (\i x -> fn x (if i==n then Just c else Nothing)) list
  Nothing -> map (\x -> fn x Nothing) list


---- JSON

toJson : (a -> String) -> [a] -> String
toJson fn list = "["
  ++ (join "," <| map fn list)
  ++ "]"