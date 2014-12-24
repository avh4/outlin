module Outline.RichText.Span.Render
  ( toHtml, valueToHtml, zipperToHtml
  ) where

import Outline.RichText.Span.Model (..)
import Html
import Html (Html)
import Basics
import App.Render.String

toHtml : (a -> Html) -> (Type, a) -> Html
toHtml fn (t,s) = case t of
  Normal -> fn s
  Bold -> Html.node "b" [] [ fn s ]
  --TODO: more cases
  _ -> Html.text <| "(?? " ++ (Basics.toString s) ++ " ??)"

valueToHtml : Value -> Html
valueToHtml = toHtml Html.text

zipperToHtml : Zipper -> Html
zipperToHtml = toHtml App.Render.String.toHtml
