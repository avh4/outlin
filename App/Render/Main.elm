module App.Render.Main (render) where

import Outline.Document.Model (..)
import Core.Action
import Core.Action (..)
import Core.Array
import Outline.Entry as Entry
import Outline.Scratch.Model as Scratch
import Text (asText, plainText)
import Color (..)
import Graphics.Element (..)
import App.Render.Scratch as Scratch
import App.Render.Outline as Outline
import List
import Graphics.Input (clickable)
import Signal

tab : Signal.Channel String -> String -> Element
tab channel label = label
  |> plainText
  |> clickable (Signal.send channel label)

tabs : Signal.Channel String -> List String -> String -> List String -> Element
tabs channel l sel r = flow right
  (  (List.map (tab channel) l)
  ++ [sel |> asText |> color yellow]
  ++ (List.map (tab channel) r)
  )

render : Signal.Channel String -> (Int,Int) -> Zipper -> Element
render tabChannel (w,h) z = case z of
  InScratch sZip _ -> flow down
    [ tabs tabChannel [] "Scratch" ["Tasks"]
    , Scratch.render sZip
    ]
  InOutline sVal eZip -> flow down
    [ tabs tabChannel ["Scratch"] "Tasks" []
    , Outline.render (w,h-50) eZip
    ]
