module Constants where

import DataTypes

import SDL
import Foreign.C.Types

red :: Color
red = SDL.V4 255 0 0 0

white :: Color
white = SDL.V4 255 255 255 0

blue :: Color
blue = SDL.V4 0 0 255 0

black :: Color
black = SDL.V4 0 0 0 0

green :: Color
green = SDL.V4 32 194 14 0

defaultColor :: Color
defaultColor = white

defaultFontSize :: FontSize
defaultFontSize = 12

{- The distance between the borders of a window and newly placed content
   e.g Paragraphs, images etc -}
borderDistance :: CInt
borderDistance = 10
