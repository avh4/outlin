module Keys where

data KeyInput =
  Enter | Backspace |
  Left | Right | Up | Down |
  Character String |
  Command String |
  Unrecognized String

fromPresses : String -> KeyInput
fromPresses string = case string of
  "\r" -> Enter
  _ -> Character string

fromDowns : Int -> KeyInput
fromDowns code = case code of
  8 -> Backspace
  13 -> Enter
  37 -> Left
  38 -> Up
  39 -> Right
  40 -> Down
  _ -> Unrecognized (show code)

fromMeta : Int -> KeyInput
fromMeta code = case code of
  65 -> Command "a"
  68 -> Command "d"
  _ -> Unrecognized ("Meta-" ++ show code)