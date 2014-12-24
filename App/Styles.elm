module App.Styles where

import Text
import Rectified (..)
import Color as C

margin = 8

highlight = grey 70
background = grey 85
panel = grey 98

plain = Text.defaultStyle
bold =  { plain | bold <- True }
dim = { plain | color <- C.hsl 0 0 0.8 }
hint = { plain | italic <- True, color <- C.hsl 0 0 0.7 }
