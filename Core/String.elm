module Core.String (Cursor, insertAction, backspace, goLeft, goRight, render, toJson) where

import Core.Action (Action)
import Core.Action as Action
import String
import Regex (..)
import Html (Html, node, text)
import Html.Attributes (class)

type Value = String
type Cursor = Int
type Subs = {}

update char value selection =
  (String.left selection value)
  ++ char
  ++ (String.dropLeft selection value)

move char value selection =
  selection + String.length char

insertAction : String -> Action String Cursor
insertAction s = Action (update s) (move s)

bback : String -> Cursor -> (String, Cursor)
bback v c = case (v,c) of
  (_, 0) -> (v,c)
  _ -> (String.left (c-1) v ++ String.dropLeft c v, c-1)

backspace : Action String Cursor
backspace = Action.split bback

goLeft = Action.nav (\_ c -> if c > 0 then c-1 else c)
goRight = Action.nav (\v c -> min (String.length v) (c+1))

render : String -> Maybe Cursor -> Html
render value msel = case msel of
  Just cursor -> node "span" [] [
    text <| String.left cursor value,
    node "span" [ class "cursor" ] [ text "^" ],
    text <| String.dropLeft cursor value ]
  Nothing -> node "span" [] [ text value ]


---- JSON

walk : (Value -> a) -> Subs -> Value -> a
walk fn _ = fn

quoteQuote = replace All (regex "\"") (\_ -> "&quot;")
quoteNewline = replace All (regex "\n") (\_ -> "\\n")

quote s = s |> quoteQuote |> quoteNewline

toJson : String -> String
toJson = walk (\s -> "\"" ++ quote s ++ "\"") {}