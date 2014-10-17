module Main where

import Html (Html, text, node, toElement)
import Html.Attributes (class)

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

data KeyInput = Left | Right | Character String | Nothing

fromKeyCode : Keyboard.KeyCode -> KeyInput
fromKeyCode key = case key of
  37 -> Left
  39 -> Right
  _ -> Character <| String.fromList [Char.fromCode key]

keys : Signal KeyInput
keys = lift fromKeyCode Keyboard.lastPressed

type Cursor = Int
type Model = { string:String, selection:Cursor }

goLeft start = start - 1

goRight start = start + 1

doKey {string,selection} char = {
  string=
    (String.left selection string)
    ++ char
    ++ (String.dropLeft selection string),
  selection= selection+1 }

apk : KeyInput -> Model -> Model
apk key last = case key of
  Left -> { last | selection <- goLeft last.selection }
  Right -> { last | selection <- goRight last.selection }
  Character s -> doKey last s
  Nothing -> last

aa = foldp apk (Model "Aaron" 2) keys

ff : Model -> Element
ff content = toElement 800 600 <| node "div" [] [
  text <| String.left content.selection content.string,
  text "|*|",
  text <| String.dropLeft content.selection content.string ]

main = ff <~ aa