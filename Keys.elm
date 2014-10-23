module Keys where

import Keyboard
import String
import Char
import Debug

data KeyInput = Enter | Left | Right | Up | Down | Character String | Nothing

fromInputs : Keyboard.KeyCode -> Bool -> KeyInput
fromInputs key isMeta = case isMeta of
  True -> Nothing
  False -> fromKeyCode key

fromKeyCode : Keyboard.KeyCode -> KeyInput
fromKeyCode key = case Debug.watch "key" key of
  13 -> Enter
  37 -> Left
  38 -> Up
  39 -> Right
  40 -> Down
  91 -> Nothing -- Meta
  _ -> Character <| Debug.watch "char" <| String.fromList [Char.fromCode key]

lastPressed : Signal KeyInput
lastPressed = lift2 fromInputs Keyboard.lastPressed Keyboard.meta