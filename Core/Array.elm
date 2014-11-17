module Core.Array (Value, Zipper, toValue, do, split, toJson, firstZipper, lastZipper, remove, map, active, zipper, append, prepend, mapAt, firstZipperThat, lastZipperThat, zipperAt, moveUp, moveDown, update, countLeft, countRight, lefts, rights, firstZipperM, goPrev, goNext) where

import Core.Action as Action
import List

type Value v = [v]
type Zipper v z = ([v],z,[v])
type Result v z = Action.Result (Value v) (Zipper v z)

replaceAt : a -> Int -> [a] -> [a]
replaceAt a index list =
  indexedMap (\i item -> if i == index then a else item) list

at : Int -> [a] -> a
at i list = list |> drop i |> head

toValue : (z -> v) -> Zipper v z -> Value v
toValue fn (left,cur,right) = reverse left ++ [fn cur] ++ right

countLeft : Zipper v z -> Int -- TODO: removing the type annotation causes compile errors
countLeft (left,_,_) = length left

countRight : Zipper v z -> Int -- TODO: removing the type annotation causes compile errors
countRight (_,_,right) = length right

lefts : Zipper v z -> [v]
lefts (left,_,_) = reverse left

rights : Zipper v z -> [v]
rights (_,_,right) = right

mapAt : Int -> (v -> v) -> Value v -> Value v
mapAt n fn vs = indexedMap (\i v -> if i == n then fn v else v) vs

append : v -> Value v -> Value v
append v vs = vs ++ [v]

prepend : v -> Value v -> Value v
prepend v vs = v :: vs

active : Zipper v z -> z
active (_,z,_) = z

zipper : [v] -> z -> [v] -> Zipper v z
zipper left cur right = (left,cur,right)

zipperAt : Int -> (v -> z) -> Value v -> Zipper v z
zipperAt i fn vs =
  ( vs |> take i |> reverse
  , vs |> drop i |> head |> fn
  , vs |> drop (i+1)
  )

-- TODO: use firstZipperM instead? or rename to firstZipper! ?
firstZipper : (v -> z) -> Value v -> Zipper v z
firstZipper fn (cur :: tail) = ([],fn cur,tail)

firstZipperM : (v -> z) -> Value v -> Maybe (Zipper v z)
firstZipperM fn vs = case vs of
  (head :: tail) -> Just ([],fn head,tail)
  [] -> Nothing

firstZipperThat : (v -> Maybe z) -> Value v -> Maybe (Zipper v z)
firstZipperThat fn vs = case vs of
  (head::tail) -> case fn head of
    Just zipper -> Just ([],zipper,tail)
    Nothing -> case firstZipperThat fn tail of
      Just (left,cur,right) -> Just (head::left,cur,right)
      Nothing -> Nothing
  [] -> Nothing

lastZipper : (v -> z) -> Value v -> Zipper v z
lastZipper fn list = let (cur :: tail) = reverse list in (tail,fn cur,[])

lastZipperThat : (v -> Maybe z) -> Value v -> Maybe (Zipper v z)
lastZipperThat fn vs = case firstZipperThat fn (reverse vs) of
  Nothing -> Nothing
  Just (left,cur,right) -> Just (right,cur,left)

remove : (v -> z) -> Zipper v z -> Maybe (Zipper v z)
remove fn (left,cur,right) =
  case right of
    (next :: tail) -> Just (left, fn next, tail)
    [] -> case left of
      (next :: tail) -> Just (tail, fn next, right)
      [] -> Nothing

-- TODO: instead of passing in nextCursor/prevCursor eagerly, can we flip it around so that there is an ActionResult that has a function : c -> Array.Cursor c
do : (z -> v) -> (v -> z) -> (v -> z) -> (z -> Action.Result v z) -> Zipper v z -> Result v z
do toVal nextFn prevFn action (left,cur,right) = case action cur of
  Action.Update new -> Action.Update (left,new,right)
  Action.Split newLeft new newRight -> Action.Update (reverse newLeft ++ left,new,newRight ++ right)
  Action.Delete -> case right of
    (next :: tail) -> Action.Update (left, nextFn next, tail)
    [] -> case left of
      (next :: tail) -> Action.Update (tail, prevFn next, right)
      [] -> Action.Delete
  Action.EnterNext -> case right of
    (next :: tail) -> Action.Update (toVal cur :: left, nextFn next, tail)
    [] -> Action.EnterNext
  Action.EnterPrev -> case left of
    (next :: tail) -> Action.Update (tail, prevFn next, toVal cur :: right)
    [] -> Action.EnterPrev
  Action.NoChange -> Action.NoChange

split_ : (z -> (v, z)) -> z -> Action.Result v z
split_ fn z = case fn z of
  (v1, new) -> Action.Split [v1] new []

split : (z -> v) -> (v -> z) -> (v -> z) -> (z -> (v, z)) -> Zipper v z -> Result v z
split toVal nextCursor prevCursor fn = do toVal nextCursor prevCursor (split_ fn)

moveUp : Zipper v z -> Maybe (Zipper v z)
moveUp (left,cur,right) = case left of
  [] -> Nothing
  (head::tail) -> Just (tail,cur,head::right)

moveDown : Zipper v z -> Maybe (Zipper v z)
moveDown (left,cur,right) = case right of
  [] -> Nothing
  (head::tail) -> Just (head::left,cur,tail)

update : z -> Zipper v z -> Zipper v z
update new (left,_,right) = (left,new,right)

map : (v -> a) -> (z -> a) -> Zipper v z -> [a]
map valueFn zipperFn (left,z,right) =
  List.map valueFn (reverse left)
  ++ [zipperFn z]
  ++ List.map valueFn right

goPrev : (z -> v) -> (v -> z) -> Zipper v z -> Maybe (Zipper v z)
goPrev toVal fn (left,cur,right) = case left of
  [] -> Nothing
  (head :: tail) -> Just (tail, fn head, toVal cur :: right)

goNext : (z -> v) -> (v -> z) -> Zipper v z -> Maybe (Zipper v z)
goNext toVal fn (left,cur,right) = case right of
  [] -> Nothing
  (head :: tail) -> Just (toVal cur :: left, fn head, tail)

---- JSON

walk : (Value b -> c) -> (a -> b) -> Value a -> c
walk wrapFn child list = wrapFn <| List.map child list

toJson : (a -> String) -> [a] -> String
toJson fn = walk (\vs -> "[" ++ (join "," vs) ++ "]") fn
