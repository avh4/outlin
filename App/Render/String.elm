module App.Render.String (render) where

import Core.String
import Graphics.Element (..)
import Text (plainText)

render : (String -> Element) -> Core.String.Zipper -> Element
render fn z = case Core.String.toTuple z of
  (l,r) -> flow right
    [ fn l
    , plainText "^"
    , fn r
    ]