module Core.String (Value, Zipper, Result, insert, backspace, goLeft, goRight, delete, split, selectToStart, selectToEnd, toJson, startZipper, endZipper, toValue, zipper, zipperAt, decoder) where

import Core.Action as Action
import String
import Regex (..)
import Html (Html, node, text)
import Html.Attributes (class)
import Json.Decode

type alias Value = String
type alias Zipper = (String,String,String)
type alias Result = Action.Result Value Zipper

startZipper : Value -> Zipper
startZipper v = ("","",v)

endZipper : Value -> Zipper
endZipper v = (v,"","")

zipper : String -> String -> Zipper
zipper left right = (left,"",right)

zipperAt : Int -> Value -> Zipper
zipperAt i s = (String.left i s, "", String.dropLeft i s)

toValue : Zipper -> Value
toValue (left,sel,right) = left ++ sel ++ right

insert : String -> Zipper -> Result
insert s (left,sel,right) = Action.Update (left ++ s, "", right)

backspace : Zipper -> Result
backspace z = case z of
  ("", "", _) -> Action.NoChange
  (left, "", right) -> Action.Update (String.dropRight 1 left, "", right)
  (left, _, right) -> Action.Update (left, "", right)

goLeft z = case z of
  ("", "", _) -> Action.NoChange
  (left, "", right) -> Action.Update (String.dropRight 1 left, "", String.right 1 left ++ right)
  (left, sel, right) -> Action.Update (left, "", sel ++ right)

goRight z = case z of
  (_, "", "") -> Action.NoChange
  (left, "", right) -> Action.Update (left ++ String.left 1 right, "", String.dropLeft 1 right)
  (left, sel, right) -> Action.Update (left ++ sel, "", right)

delete z = Action.Delete

split : Zipper -> Result
split (left,_,right) = Action.Split [left] (startZipper right) []

selectToStart : Zipper -> Result
selectToStart (left,sel,right) = Action.Update ("", left ++ sel, right)

selectToEnd : Zipper -> Result
selectToEnd (left,sel,right) = Action.Update (left, sel ++ right, "")

---- JSON

walk : (Value -> a) -> Value -> a
walk fn = fn

quoteQuote = replace All (regex "\"") (\_ -> "&quot;")
quoteNewline = replace All (regex "\n") (\_ -> "\\n")

quote s = s |> quoteQuote |> quoteNewline

toJson : String -> String
toJson = walk (\s -> "\"" ++ quote s ++ "\"")

decoder : Json.Decode.Decoder Value
decoder = Json.Decode.string
