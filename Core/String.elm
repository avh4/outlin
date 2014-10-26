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

update char value cursor =
  (String.left cursor value)
  ++ char
  ++ (String.dropLeft cursor value)

move char value cursor =
  cursor + String.length char

insertAction : String -> Action String Cursor
insertAction s v c = Action.Update (update s v c) (move s v c)

backspace : Action String Cursor
backspace v c = case (v,c) of
  (_, 0) -> Action.Update v c -- TODO: Action.Nothing
  _ -> Action.Update (String.left (c-1) v ++ String.dropLeft c v) (c-1)

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
