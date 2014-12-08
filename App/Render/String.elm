module App.Render.String (render) where

import Core.String
import Graphics.Element (..)
import Html (..)
import Html.Attributes (..)
import String

render : Core.String.Zipper -> Element
render (value,cursor) = node "span" [] [
  text <| String.left cursor value,
  node "span" [ class "cursor" ] [ text "^" ],
  text <| String.dropLeft cursor value ]
  |> toElement 100 100