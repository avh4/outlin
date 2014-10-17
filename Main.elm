module Main where

import Html (Html, text, node, toElement)
import Html.Attributes (class)

import Graphics.Input.Field (..)
import Graphics.Input (..)
import String
import Keyboard
import Char

type Document = [Block]

data Block =
  WithSelection BlockContent |
  NoSelection BlockContent

data BlockContent =
  Heading Int Span | -- level, content
  Paragraph Span | -- content
  CodeBlock (Maybe String) String -- language, content

data Span =
  Plain String

---- Rendering

toHtml : Document -> Html
toHtml d = node "div" [] <| map blockToHtml d

blockToHtml : Block -> Html
blockToHtml b = case b of
  WithSelection c -> node "div" [ class "selected" ] [ blockContentToHtml c ]
  NoSelection c -> blockContentToHtml c

blockContentToHtml b = case b of
  Heading 1 s -> node "h1" [] [ spanToHtml s ]
  Heading _ s -> node "h2" [] [ spanToHtml s ]
  Paragraph s -> node "p" [] [ spanToHtml s ]
  CodeBlock _ s -> node "code" [] [ text s ]

spanToHtml : Span -> Html
spanToHtml span = case span of
  Plain s -> text s

---- App

exampleDoc = [
  WithSelection <| Heading 1 (Plain "Welcome to Elm"),
  NoSelection <| Paragraph (Plain "A functional reactive language for interactive applications"),
  NoSelection <| CodeBlock (Just "elm") "main = asText \"Hello World\""
  ]

--main = toElement 800 600 <| toHtml exampleDoc

---- Test input

type Model = Content

goLeft {start,end,direction} = {start=start-1, end=end, direction=direction}

goRight {start,end,direction} = {start=start+1, end=end, direction=direction}

doKey {string,selection} char = {
  string=
    (String.left selection.start string)
    ++ String.fromList [char]
    ++ (String.dropLeft selection.start string),
  selection=Selection (selection.start+1) (selection.end+1) Forward }

apk : Keyboard.KeyCode -> Content -> Content
apk key last = case key of
  37 -> { last | selection <- goLeft last.selection }
  39 -> { last | selection <- goRight last.selection }
  _ -> doKey last <| Char.fromCode key

aa = foldp apk (Content "Aaron" <| Selection 2 0 Forward) Keyboard.lastPressed

ff : Model -> Element
ff content = toElement 800 600 <| node "div" [] [
  text <| String.left content.selection.start content.string,
  text "|*|",
  text <| String.dropLeft content.selection.start content.string ]

--main = field defaultStyle aa.handle identity "__" <~ aa.signal
main = ff <~ aa