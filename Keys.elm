module Keys where

import Debug

data KeyInput = Enter | Left | Right | Up | Down | Character String | Nothing

fromPresses : String -> KeyInput
fromPresses string = Character string

fromDowns : Int -> KeyInput
fromDowns key = case Debug.watch "key" key of
  8 -> Left
  13 -> Enter
  37 -> Left
  38 -> Up
  39 -> Right
  40 -> Down
  _ -> Nothing
