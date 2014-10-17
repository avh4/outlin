module Main where

import Html (Html, text, node, toElement)
import Html.Attributes (class)

import String
import Keys
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

type Cursor = Int
type Model = { value:Span, selection:Cursor }

goLeft start = start - 1

goRight start = start + 1

insertInString : (String, Cursor) -> String -> (String, Cursor)
insertInString (value, selection) char =
  let newValue = (String.left selection value) ++ char ++ (String.dropLeft selection value)
      newSelection = selection + 1
    in (newValue, newSelection)

insertInSpan : (Span, Cursor) -> String -> (Span, Cursor)
insertInSpan (value,selection) char = case value of
  Plain s ->
    let (a, b) = insertInString (s, selection) char
    in (Plain a, b)

insertInModel : Model -> String -> Model
insertInModel {value,selection} char =
  let (a, b) = insertInSpan (value, selection) char
  in {value=a, selection=b}

apk : Keys.KeyInput -> Model -> Model
apk key last = case key of
  Keys.Left -> { last | selection <- goLeft last.selection }
  Keys.Right -> { last | selection <- goRight last.selection }
  Keys.Character s -> insertInModel last s
  Keys.Nothing -> last

aa = foldp apk (Model (Plain "Aaron") 2) Keys.lastPressed

renderSpan : Model -> Html
renderSpan content = case content.value of
  Plain string -> node "div" [] [
    text <| String.left content.selection string,
    node "span" [ class "cursor" ] [ text "|" ],
    text <| String.dropLeft content.selection string ]

main = (toElement 800 600) <~ (renderSpan <~ aa)