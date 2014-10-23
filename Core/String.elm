module Core.String where

import String
import Html (Html, node, text)
import Html.Attributes (class)

type Cursor = Int

update value selection char =
  (String.left selection value)
  ++ char
  ++ (String.dropLeft selection value)

move value selection char =
  selection + 1 -- TODO length of char

goLeft cur = cur - 1

goRight cur = cur + 1

render : String -> Maybe Cursor -> Html
render value msel = case msel of
  Just cursor -> node "span" [] [
    text <| String.left cursor value,
    node "span" [ class "cursor" ] [ text "^" ],
    text <| String.dropLeft cursor value ]
  Nothing -> node "span" [] [ text value ]
