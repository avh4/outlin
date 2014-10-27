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
  80 -> Command "p"
  49 -> Command "1"
  50 -> Command "2"
  51 -> Command "3"
  52 -> Command "4"
  53 -> Command "5"
  54 -> Command "6"
  55 -> Command "7"
  _ -> Unrecognized ("Meta-" ++ show code)