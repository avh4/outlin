module Main where

import Html (Html, text, node, toElement)
import Html.Attributes (class)

import String
import Keys
import Char
import Debug
import Markdown.Span as Span
import Markdown.Block as Block

type Document = [Block.Block]

---- App

-- exampleDoc = [
--   Heading 1 (Plain "Welcome to Elm"),
--   Paragraph (Plain "A functional reactive language for interactive applications"),
--   CodeBlock (Just "elm") "main = asText \"Hello World\""
--   ]

--main = toElement 800 600 <| toHtml exampleDoc

---- Test input

type DocumentCursor = (Int, Block.Cursor)
type Model = { value:Document, selection:DocumentCursor }

goLeft (n, start) = (n, start - 1)

goRight (n, start) = (n, start + 1)

type Foo a b = {
  update: (a, b) -> String -> a,
  move: (a, b) -> String -> b
  }

liftArrayTuple : ([a], b) -> [(a,b)]
liftArrayTuple (aa, b) =
  map (\a -> (a,b)) aa

documenter : Foo Document DocumentCursor
documenter =
  { update = \(value, cursor) char ->
    changeAt (\(s,c) -> Block.update (s,snd c) char) (\(s,c) -> s) (fst cursor) (liftArrayTuple (value, cursor))
  , move = \(value, cursor) char ->
    (fst cursor, Block.move (head value, snd cursor) char)
  }

changeAt : (a -> b) -> (a -> b) -> Int -> [a] -> [b]
changeAt fn1 fn2 index list =
  indexedMap (\i item -> if i == index then fn1 item else fn2 item) list

insertInModel : Model -> String -> Model
insertInModel {value,selection} char =
  let a = documenter.update (value, selection) char
      b = documenter.move (value, selection) char
  in {value=a, selection=b}

-- INPUT

apk : Keys.KeyInput -> Model -> Model
apk key last = case key of
  Keys.Left -> { last | selection <- goLeft last.selection }
  Keys.Right -> { last | selection <- goRight last.selection }
  Keys.Enter -> last
  Keys.Character s -> insertInModel last s
  Keys.Nothing -> last

-- RENDER

renderDocument : Document -> DocumentCursor -> Html
renderDocument blocks cursor =
  changeAt (\s -> Block.render s <| Just <| snd cursor) (\s -> Block.render s Nothing) (fst cursor) blocks
  |> node "div" []

renderModel : Model -> Html
renderModel m = renderDocument m.value m.selection


aa = foldp apk (Model [
  Block.Heading 1 <| Span.Plain "Welcome to Elm",
  Block.Paragraph <| Span.Plain "A functional reactive language for interactive applications",
  Block.CodeBlock (Just "elm") "main = asText \"Hello World\""
  ] (1, 2)) Keys.lastPressed

main = (toElement 800 600) <~ (renderModel <~ aa)