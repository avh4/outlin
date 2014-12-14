module Core.Array (Value, Zipper, toValue, do, toJson, firstZipper, lastZipper, remove, map, indexedMap, active, zipper, append, prepend, mapAt, firstZipperThat, lastZipperThat, zipperAt, zipperAtM, moveUp, moveDown, update, countLeft, countRight, lefts, rights, firstZipperM, goPrev, goNext) where

import Core.Action as Action
import List
import List (..)
import String

type alias Value v = List v
type alias Zipper v z = (List v,z,List v)
type alias Result v z = Action.Result (Value v) (Zipper v z)

replaceAt : a -> Int -> List a -> List a
replaceAt a index list =
  List.indexedMap (\i item -> if i == index then a else item) list

at : Int -> List a -> a
at i list = list |> List.drop i |> List.head

toValue : (z -> v) -> Zipper v z -> Value v
toValue fn (left,cur,right) = List.reverse left ++ [fn cur] ++ right

countLeft : Zipper v z -> Int -- TODO: removing the type annotation causes compile errors
countLeft (left,_,_) = List.length left

countRight : Zipper v z -> Int -- TODO: removing the type annotation causes compile errors
countRight (_,_,right) = List.length right

lefts : Zipper v z -> List v
lefts (left,_,_) = List.reverse left

rights : Zipper v z -> List v
rights (_,_,right) = right

mapAt : Int -> (v -> v) -> Value v -> Value v
mapAt n fn vs = List.indexedMap (\i v -> if i == n then fn v else v) vs

append : v -> Value v -> Value v
append v vs = vs ++ [v]

prepend : v -> Value v -> Value v
prepend v vs = v :: vs

active : Zipper v z -> z
active (_,z,_) = z

zipper : List v -> z -> List v -> Zipper v z
zipper left cur right = (left,cur,right)

-- TODO: should return Maybe; replace zipperAtM
zipperAt : Int -> (v -> z) -> Value v -> Zipper v z
zipperAt i fn vs =
  ( vs |> take i |> reverse
  , vs |> drop i |> head |> fn
  , vs |> drop (i+1)
  )

zipperAtM : Int -> (v -> z) -> Value v -> Maybe (Zipper v z)
zipperAtM i fn vs = case List.isEmpty (vs |> drop i) of
  True -> Nothing
  False -> Just
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


-- TODO: Needs to return a Maybe
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

map : (v -> a) -> (z -> a) -> Zipper v z -> List a
map valueFn zipperFn (left,z,right) =
  List.map valueFn (reverse left)
  ++ [zipperFn z]
  ++ List.map valueFn right

indexedMap : (Int -> v -> a) -> (Int -> z -> a) -> Zipper v z -> List a
indexedMap valueFn zipperFn (left,z,right) =
  let leftCount = List.length left
  in
    List.indexedMap valueFn (reverse left)
    ++ [zipperFn leftCount z]
    ++ List.indexedMap (\i v -> valueFn (leftCount+1+i) v) right

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

toJson : (a -> String) -> List a -> String
toJson fn = walk (\vs -> "[" ++ (String.join "," vs) ++ "]") fn
