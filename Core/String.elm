module Core.String
  ( Zipper, Result
  , toValue, destructure
  , startZipper, endZipper, allZipper, rangeZipper
  , goLeft, goRight, moveToStartOfLine, moveToEndOfLine
  , insert, backspace, delete, split
  , selectToStart, selectToEnd, selectToStartOfLine, selectToEndOfLine, selectLeft, selectRight
  , zipper, zipperAt
  ) where

import Core.Action (..)
import String
import List
import Regex (..)
import Html (Html, node, text)
import Html.Attributes (class)
import Json.Decode

type alias Zipper = (String,String,String)
type alias Result = ActionResult String Zipper

toValue : Zipper -> String
toValue (left,sel,right) = left ++ sel ++ right

destructure : Zipper -> (String,String,String)
destructure z = z

startZipper : String -> Zipper
startZipper v = ("","",v)

endZipper : String -> Zipper
endZipper v = (v,"","")

allZipper : String -> Zipper
allZipper v = ("",v,"")

rangeZipper : (Int,Int) -> String -> Zipper
rangeZipper (start,length) s =
  ( String.left start s
  , String.dropLeft start s |> String.left length
  , String.dropLeft start s |> String.dropLeft length
  )

zipper : String -> String -> Zipper
zipper left right = (left,"",right)

zipperAt : Int -> String -> Zipper
zipperAt i s = (String.left i s, "", String.dropLeft i s)

insert : String -> Zipper -> Result
insert s (left,sel,right) = Update (left ++ s, "", right)

backspace : Zipper -> Result
backspace z = case z of
  ("", "", _) -> NoChange
  (left, "", right) -> Update (String.dropRight 1 left, "", right)
  (left, _, right) -> Update (left, "", right)

goLeft z = case z of
  ("", "", _) -> EnterPrev
  (left, "", right) -> Update (String.dropRight 1 left, "", String.right 1 left ++ right)
  (left, sel, right) -> Update (left, "", sel ++ right)

goRight z = case z of
  (_, "", "") -> EnterNext
  (left, "", right) -> Update (left ++ String.left 1 right, "", String.dropLeft 1 right)
  (left, sel, right) -> Update (left ++ sel, "", right)

moveToStartOfLine : Zipper -> Result
moveToStartOfLine (left, sel, right) = case takeLast "\n" left of
  (rest, a) -> Update (rest, "", a ++ sel ++ right)

moveToEndOfLine : Zipper -> Result
moveToEndOfLine (left, sel, right) = case takeFirst "\n" right of
  (a, rest) -> Update (left ++ sel ++ a, "", rest)

delete z = Delete

split : Zipper -> Result
split (left,_,right) = Split [left] (startZipper right) []

selectLeft : Zipper -> Result
selectLeft z = case z of
  ("", _, _) -> NoChange
  (left,sel,right) -> Update (String.dropRight 1 left, String.right 1 left ++ sel, right)

selectRight : Zipper -> Result
selectRight z = case z of
  (_, _, "") -> NoChange
  (left,sel,right) -> Update (left, sel ++ String.left 1 right, String.dropLeft 1 right)

selectToStart : Zipper -> Result
selectToStart (left,sel,right) = Update ("", left ++ sel, right)

selectToEnd : Zipper -> Result
selectToEnd (left,sel,right) = Update (left, sel ++ right, "")

takeFirst : String -> String -> (String, String)
takeFirst needle s = if
  | String.contains needle s ->
    case String.split needle s of
      [] -> ("", "")
      (first::rest) -> (first, needle ++ String.join needle rest)
  | True -> (s, "")

takeLast : String -> String -> (String, String)
takeLast needle s = if
  | String.contains needle s ->
    case String.split needle s of
    [] -> ("", "")
    tokens -> case List.reverse tokens of
      (last::rest) -> (String.join needle (List.reverse rest) ++ needle, last)
  | True -> ("", s)

selectToStartOfLine : Zipper -> Result
selectToStartOfLine (left,sel,right) = case takeLast "\n" left of
  (rest, last) -> Update (rest, last ++ sel, right)

selectToEndOfLine : Zipper -> Result
selectToEndOfLine (left,sel,right) = Update (left, sel ++ right, "") -- TODO
