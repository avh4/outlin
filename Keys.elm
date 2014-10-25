module Keys where

import Debug

data KeyInput =
  Enter | Backspace |
  Left | Right | Up | Down |
  Character String |
  Nothing

fromPresses : String -> KeyInput
fromPresses string = case string of
  "\r" -> Enter
  _ -> Character string

fromDowns : Int -> KeyInput
fromDowns key = case Debug.watch "key" key of
  8 -> Backspace
  13 -> Enter
  37 -> Left
  38 -> Up
  39 -> Right
  40 -> Down
  _ -> fst (Nothing, Debug.log "dropped key" key)
