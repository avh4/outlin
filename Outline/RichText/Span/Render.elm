module Outline.RichText.Span.Render
  ( toHtml, valueToHtml, zipperToHtml
  ) where

import Html
import Html (Html)
import Basics
import App.Render.String
import RichText (..)

toHtml : (a -> Html) -> (SpanType, a) -> Html
toHtml fn (t,s) = case t of
  Normal -> fn s
  Bold -> Html.node "b" [] [ fn s ]
  --TODO: more cases
  _ -> Html.text <| "(?? " ++ (Basics.toString s) ++ " ??)"

valueToHtml : Span -> Html
valueToHtml = toHtml Html.text

zipperToHtml : SpanZipper -> Html
zipperToHtml = toHtml App.Render.String.toHtml
