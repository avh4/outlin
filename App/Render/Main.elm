module App.Render.Main (render) where

import Outline.Document.Model (..)
import Core.Action
import Core.Action (..)
import Core.Array
import Outline.Entry as Entry
import Outline.Scratch.Model as Scratch
import App.Render.Scratch as Scratch
import App.Render.Outline as Outline
import App.Render.Notes as Notes
import List
import Signal
import Text

import Rectified (..)
import App.Styles (..)

tab channel (isSelected,label) = case isSelected of
  True -> centeredText bold label |> highlight
  False -> centeredText plain label
    |> clickable (Signal.send channel label)

tabbar : Signal.Channel String -> List (Bool,String) -> Element
tabbar channel vs = row 0 (tab channel) vs |> panel

tabNames = ["Scratch", "Tasks", "Notes"]

selectTab name = List.map (\t -> (name == t, t)) tabNames
tabName z = case z of
  InScratch _ -> "Scratch"
  InOutline _ -> "Tasks"
  InNotesArchive _ -> "Notes"

body scratchChannel processScratchChannel newScratchChannel z = case z of
  InScratch r -> Scratch.render scratchChannel processScratchChannel newScratchChannel r.scratch
  InOutline r -> Outline.render r.outline
  InNotesArchive r -> Notes.render r.notes

render : Signal.Channel String -> Signal.Channel Int -> Signal.Channel () -> Signal.Channel () -> Zipper -> Element
render tabChannel scratchChannel processScratchChannel newScratchChannel z =
  top 60 0
    (tabbar tabChannel (selectTab <| tabName z))
    (body scratchChannel processScratchChannel newScratchChannel z)
  |> background
