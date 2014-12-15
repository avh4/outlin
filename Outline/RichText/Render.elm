module Outline.RichText.Render (toElement, toHtml) where

import Outline.RichText.Span.Model (Type(..))
import Outline.RichText.Model (..)
import Html
import Html (Html)
import List
import Graphics.Element (..)

toHtml : (a -> Html) -> (Type,a) -> Html
toHtml fn (t,s) = case t of
  Normal -> fn s
  Bold -> Html.node "b" [] [ fn s ]
  --TODO: more cases
  _ -> Html.text <| "(?? " ++ (toString s) ++ " ??)"

toElement : Int -> Int -> Value -> Element
toElement w h v =
  Html.node "div" [] (List.map (toHtml Html.text) v)
  |> Html.toElement w h
