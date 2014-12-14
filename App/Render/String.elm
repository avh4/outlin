module App.Render.String (render) where

import Core.String
import Graphics.Element (..)
import Html (..)
import Html.Attributes (..)
import String
import List

line : String -> Html
line string = node "p" [] [ text string ]

lines : String -> List Html
lines stringWithNewlines = stringWithNewlines
  |> String.split "\n"
  |> List.map line

render : Core.String.Zipper -> Element
render (left,sel,right) = node "span"
  [ style [ ("white-space", "pre-wrap")]
  ]
  [ text <| left
  , node "span" [ class "cursor" ] [ text "^" ]
  , node "span" [ class "selection" ] [ text sel ]
  , text <| right
  ]
  |> toElement 100 100