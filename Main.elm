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
type Model = { value:[Span], selection:Cursor }

goLeft start = start - 1

goRight start = start + 1

type Foo a = {
  update: (a, Cursor) -> String -> a,
  move: (a, Cursor) -> String -> Cursor
  }

stringer : Foo String
stringer =
  { update = \(value, selection) char ->
    (String.left selection value)
    ++ char
    ++ (String.dropLeft selection value)
  , move = \(value, selection) char ->
    selection + 1 -- TODO lenght of char
  }

spanner : Foo Span
spanner =
  { update = \(value, selection) char -> case value of
    Plain s -> Plain <| stringer.update (s, selection) char
  , move = \(value, selection) char -> case value of
    Plain s -> stringer.move (s, selection) char
  }

liftArrayTuple : ([a], b) -> [(a,b)]
liftArrayTuple (aa, b) =
  map (\a -> (a,b)) aa

spanser : Foo [Span]
spanser =
  { update = \(value, selection) char ->
    map (\x -> spanner.update x char) (liftArrayTuple (value, selection))
  , move = \(value, selection) char ->
    spanner.move (head value, selection) char
  }

insertInModel : Model -> String -> Model
insertInModel {value,selection} char =
  let a = spanser.update (value, selection) char
      b = spanser.move (value, selection) char
  in {value=a, selection=b}

-- INPUT

apk : Keys.KeyInput -> Model -> Model
apk key last = case key of
  Keys.Left -> { last | selection <- goLeft last.selection }
  Keys.Right -> { last | selection <- goRight last.selection }
  Keys.Character s -> insertInModel last s
  Keys.Nothing -> last

-- RENDER

renderSpan : Span -> Cursor -> Html
renderSpan span cursor = case span of
  Plain string -> node "div" [] [
    text <| String.left cursor string,
    node "span" [ class "cursor" ] [ text "|" ],
    text <| String.dropLeft cursor string ]

renderSpans : [Span] -> Cursor -> Html
renderSpans spans cursor = map (\s -> renderSpan s cursor) spans
  |> node "div" []

renderModel : Model -> Html
renderModel m = renderSpans m.value m.selection


aa = foldp apk (Model [Plain "Aaron", Plain "Boyd", Plain "Welcome"] 2) Keys.lastPressed

main = (toElement 800 600) <~ (renderModel <~ aa)