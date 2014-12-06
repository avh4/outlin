module Core.String (Value, Zipper, Result, insert, backspace, goLeft, goRight, delete, split, renderValue, renderZipper, toJson, startZipper, endZipper, toValue, split_, zipper, zipperAt, toTuple) where

import Core.Action as Action
import String
import Regex (..)
import Html (Html, node, text)
import Html.Attributes (class)

type alias Value = String
type alias Zipper = (String,Int)
type alias Result = Action.Result Value Zipper

startZipper : Value -> Zipper
startZipper v = (v,0)

endZipper : Value -> Zipper
endZipper v = (v,String.length v)

zipper : String -> String -> Zipper
zipper left right = (left ++ right, String.length left)

zipperAt : Int -> Value -> Zipper
zipperAt i s = (s, i)

toValue : Zipper -> Value
toValue (v,_) = v

toTuple : Zipper -> (String,String)
toTuple (s,i) = (String.left i s, String.dropLeft i s)

split_ : Zipper -> (Value, Zipper)
split_ (v,i) = (String.left i v, startZipper (String.dropLeft i v))

update char value cursor =
  (String.left cursor value)
  ++ char
  ++ (String.dropLeft cursor value)

move char value cursor =
  cursor + String.length char

insert : String -> Zipper -> Result
insert s (v,c) = Action.Update ((update s v c),(move s v c))

backspace : Zipper -> Result
backspace (v,c) = case (v,c) of
  (_, 0) -> Action.NoChange
  _ -> Action.Update ((String.left (c-1) v ++ String.dropLeft c v),(c-1))

goLeft = Action.nav (\_ c -> if c > 0 then c-1 else c)
goRight = Action.nav (\v c -> min (String.length v) (c+1))

delete = Action.always Action.Delete

split : Zipper -> Result
split (s,n) = Action.Split [String.left n s] (String.dropLeft n s, 0) []

renderValue : Value -> Html
renderValue value = text value

renderZipper : Zipper -> Html
renderZipper (value,cursor) = node "span" [] [
  text <| String.left cursor value,
  node "span" [ class "cursor" ] [ text "^" ],
  text <| String.dropLeft cursor value ]

---- JSON

walk : (Value -> a) -> Value -> a
walk fn = fn

quoteQuote = replace All (regex "\"") (\_ -> "&quot;")
quoteNewline = replace All (regex "\n") (\_ -> "\\n")

quote s = s |> quoteQuote |> quoteNewline

toJson : String -> String
toJson = walk (\s -> "\"" ++ quote s ++ "\"")
