module Core.String where

import String

update value selection char =
  (String.left selection value)
  ++ char
  ++ (String.dropLeft selection value)

move value selection char =
  selection + 1 -- TODO length of char