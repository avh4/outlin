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

tab : String -> Element
tab label = label |> plainText |> width 80

clickableTab channel label = tab label
  |> clickable (Signal.send channel label)

selectedTab label = tab label
  |> color yellow

tabs : Signal.Channel String -> List String -> String -> List String -> Element
tabs channel l sel r = flow right
  (  (List.map (clickableTab channel) l)
  ++ [sel |> selectedTab]
  ++ (List.map (clickableTab channel) r)
  )

render : Signal.Channel String -> Signal.Channel Int -> Signal.Channel () -> (Int,Int) -> Zipper -> Element
render tabChannel scratchChannel processScratchChannel (w,h) z = case z of
  InScratch sZip _ -> flow down
    [ tabs tabChannel [] "Scratch" ["Tasks"]
    , Scratch.render w scratchChannel processScratchChannel sZip
    ]
  InOutline sVal eZip -> flow down
    [ tabs tabChannel ["Scratch"] "Tasks" []
    , Outline.render (w,h-50) eZip
    ]
