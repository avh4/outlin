module Core.Array (Cursor, cursor, do, split, render, toJson, zipper, unzipper) where

import Core.Action as Action

type Value a = [a]
type Cursor c = (Int, c)
type Zipper a c = ([a],(a,c),[a])
type Subs a = {child:a}
type Result a c = Action.Result (Value a) (Zipper a c)

-- TODO: remove this
cursor n c = (n, c)

replaceAt : a -> Int -> [a] -> [a]
replaceAt a index list =
  indexedMap (\i item -> if i == index then a else item) list

at : Int -> [a] -> a
at i list = list |> drop i |> head

-- TODO: remove this
zipper : [a] -> (Int, c) -> Zipper a c
zipper list (i,cur) = (take i list |> reverse, (at i list, cur), drop (i+1) list)

-- TODO: remove this
unzipper : Zipper a c -> ([a], (Int,c))
unzipper (left,(current,cursor),right) = (reverse left ++ [current] ++ right,(length left,cursor))

-- TODO: instead of passing in nextCursor/prevCursor eagerly, can we flip it around so that there is an ActionResult that has a function : c -> Array.Cursor c
do : c -> (v -> c) -> ((v,c) -> Action.Result v (v,c)) -> Zipper v c -> Result v c
do nextCursor prevCursor action (left,cur,right) = case action cur of
  Action.Update newCur -> Action.Update (left,newCur,right)
  Action.Split newLeft newCur newRight -> Action.Update (reverse newLeft ++ left,newCur,newRight ++ right)
  Action.Delete -> case right of
    (next :: tail) -> Action.Update (left, (next,nextCursor), tail)
    [] -> case left of
      (next :: tail) -> Action.Update (tail, (next,prevCursor next), right)
      [] -> Action.Delete
  Action.EnterNext -> case right of
    (next :: tail) -> Action.Update (fst cur :: left, (next, nextCursor), tail)
    [] -> Action.EnterNext
  Action.EnterPrev -> case left of
    (next :: tail) -> Action.Update (tail, (next, prevCursor next), fst cur :: right)
    [] -> Action.EnterPrev
  Action.NoChange -> Action.NoChange

split_ : (v -> c -> (v, v, c)) -> (v,c) -> Action.Result v (v,c)
split_ fn = \(v,c) -> case fn v c of
  (v1, v2, innerC) -> Action.Split [v1] (v2,innerC) []

split : c -> (v -> c) -> (v -> c -> (v, v, c)) -> Zipper v c -> Result v c
split nextCursor prevCursor fn = do nextCursor prevCursor (split_ fn)

render : (val -> Maybe cur -> out) -> [val] -> Maybe (Cursor cur) -> [out]
render fn list msel = case msel of
  Just (n, c) -> indexedMap (\i x -> fn x (if i==n then Just c else Nothing)) list
  Nothing -> map (\x -> fn x Nothing) list


---- JSON

walk : (Value b -> c) -> Subs (a -> b) -> Value a -> c
walk wrapFn {child} list = wrapFn <| map child list

toJson : (a -> String) -> [a] -> String
toJson fn = walk (\vs -> "[" ++ (join "," vs) ++ "]") {child=fn}
