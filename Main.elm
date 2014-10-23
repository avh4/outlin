module Main where

import Html (Html, text, node, toElement)
import Html.Attributes (class)

import String
import Keys
import Char
import Debug
import Outline.Entry as Entry


type Document = Entry.Entry

---- App

---- Test input

type DocumentCursor = Entry.Cursor
type Model = { value:Document, selection:DocumentCursor }

liftArrayTuple : ([a], b) -> [(a,b)]
liftArrayTuple (aa, b) =
  map (\a -> (a,b)) aa

updateDocument : Document -> DocumentCursor -> String -> Document
updateDocument = Entry.update 

moveDocument : Document -> DocumentCursor -> String -> DocumentCursor
moveDocument = Entry.move
--    (fst cursor, Block.move (head value, snd cursor) char)

changeAt : (a -> b) -> (a -> b) -> Int -> [a] -> [b]
changeAt fn1 fn2 index list =
  indexedMap (\i item -> if i == index then fn1 item else fn2 item) list

insertInModel : Model -> String -> Model
insertInModel {value,selection} char =
  let a = updateDocument value selection char
      b = moveDocument  value selection char
  in {value=a, selection=b}

-- INPUT

apk : Keys.KeyInput -> Model -> Model
apk key last = case key of
  Keys.Left -> { last | selection <- Entry.goLeft last.value last.selection }
  Keys.Right -> { last | selection <- Entry.goRight last.value last.selection }
  Keys.Down -> { last | selection <- Entry.goNext last.value last.selection }
  Keys.Up -> { last | selection <- Entry.goPrev last.value last.selection }
  Keys.Enter -> last
  Keys.Character s -> insertInModel last s
  Keys.Nothing -> last

-- RENDER

renderDocument : Document -> DocumentCursor -> Html
renderDocument value cursor = Entry.render value (Just <| Debug.watch "cursor" cursor)

--  changeAt (\s -> Block.render s <| Just <| snd cursor) (\s -> Block.render s Nothing) (fst cursor) blocks
--  |> node "div" []

renderModel : Model -> Html
renderModel m = renderDocument m.value m.selection

port pressesIn : Signal String
port downsIn : Signal Int

lastPressed = merge (lift Keys.fromPresses pressesIn) (lift Keys.fromDowns downsIn)

aa = foldp apk (Model
  (Entry.Entry { text="Tasks", description="", children=[
    Entry.Entry { text="Inbox", description="", children=[] },
    Entry.Entry { text="By time (deadline)", description="", children=[
      Entry.Entry { text="daily", description="", children=[] },
      Entry.Entry { text="weekly", description="", children=[] },
      Entry.Entry { text="waiting on", description="", children=[] },
      Entry.Entry { text="monthly", description="", children=[] },
      Entry.Entry { text="yearly, etc.", description="", children=[] }
    ] },
    Entry.Entry { text="Habits", description="", children=[
      Entry.Entry { text="daily", description="", children=[] },
      Entry.Entry { text="weekly", description="", children=[] },
      Entry.Entry { text="monthly", description="", children=[] }
    ] },
    Entry.Entry { text="By priorty", description="", children=[] },
    Entry.Entry { text="By project", description="", children=[] }
    ] })
  (Entry.InText 4)) lastPressed

main = (toElement 800 600) <~ (renderModel <~ aa)