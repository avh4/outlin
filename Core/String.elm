module Core.String where

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

goLeft value cur = if cur > 0 then cur - 1 else cur

goRight value cur = if cur < String.length value then cur + 1 else cur

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