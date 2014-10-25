module Core.String where

import Core.Action (Action)
import String
import Regex (..)
import Html (Html, node, text)
import Html.Attributes (class)

type Cursor = Int

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

toAction : (v -> c -> (v, c)) -> Action v c
toAction fn = Action (\v c -> fst <| fn v c) (\v c -> snd <| fn v c)

backspace : Action String Cursor
backspace = toAction bback

goLeft value cur = if cur > 0 then cur - 1 else cur
goRight value cur = if cur < String.length value then cur + 1 else cur

goLeftAction = Action (\v _ -> v) goLeft
goRightAction = Action (\v _ -> v) goRight

render : String -> Maybe Cursor -> Html
render value msel = case msel of
  Just cursor -> node "span" [] [
    text <| String.left cursor value,
    node "span" [ class "cursor" ] [ text "^" ],
    text <| String.dropLeft cursor value ]
  Nothing -> node "span" [] [ text value ]


---- JSON

quoteQuote = replace All (regex "\"") (\_ -> "&quot;")
quoteNewline = replace All (regex "\n") (\_ -> "\\n")

quote s = s |> quoteQuote |> quoteNewline

toJson : String -> String
toJson s = "\"" ++ quote s ++ "\""