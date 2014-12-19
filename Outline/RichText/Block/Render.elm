module Outline.RichText.Block.Render
  ( valueToHtml, zipperToHtml
  ) where

import Outline.RichText.Block.Model (..)
import Outline.RichText.Span.Render as Span
import Html (..)
import Html.Attributes (..)
import List
import Core.Array

wrap t children = case t of
  Paragraph -> node "p" [] children
  Quote -> node "blockquote" [] children
  Task -> node "p" [ class "task" ] children

valueToHtml : Value -> Html
valueToHtml (t,spanValues) = wrap t (List.map Span.valueToHtml spanValues)

zipperToHtml : Zipper -> Html
zipperToHtml (t,spansZipper) = wrap t (Core.Array.map Span.valueToHtml Span.zipperToHtml spansZipper)
