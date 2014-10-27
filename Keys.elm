module Keys where

import Debug

data KeyInput =
  Enter | Backspace |
  Left | Right | Up | Down |
  Character String |
  Command String |
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

fromMeta : Int -> KeyInput
fromMeta code = case code of
  65 -> Command "a"
  68 -> Command "d"
  _ -> fst (Nothing, Debug.log "dropped key" ("Meta-" ++ show code))