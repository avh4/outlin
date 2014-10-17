module Keys where

import Keyboard
import String
import Char

data KeyInput = Left | Right | Character String | Nothing

fromKeyCode : Keyboard.KeyCode -> KeyInput
fromKeyCode key = case key of
  37 -> Left
  39 -> Right
  _ -> Character <| String.fromList [Char.fromCode key]

lastPressed : Signal KeyInput
lastPressed = lift fromKeyCode Keyboard.lastPressed