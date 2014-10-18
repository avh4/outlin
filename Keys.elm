module Keys where

import Keyboard
import String
import Char
import Debug

data KeyInput = Enter | Left | Right | Character String | Nothing

fromKeyCode : Keyboard.KeyCode -> KeyInput
fromKeyCode key = case Debug.watch "key" key of
  13 -> Enter
  37 -> Left
  39 -> Right
  _ -> Character <| Debug.watch "char" <| String.fromList [Char.fromCode key]

lastPressed : Signal KeyInput
lastPressed = lift fromKeyCode Keyboard.lastPressed