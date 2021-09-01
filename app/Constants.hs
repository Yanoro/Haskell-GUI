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

defaultBreakSize :: CInt
defaultBreakSize = 50

{- The distance between the borders of a window and newly placed content
   e.g Paragraphs, images etc -}
borderDistance :: CInt
borderDistance = 10

{- Default parameters for a window -}

defaultDimensions :: Rectangle CInt
defaultDimensions = Rectangle (SDL.P (SDL.V2 0 0)) (SDL.V2 500 500)

-- First element represents max width, the second max height
defaultMaxDimensions, defaultMinDimensions :: (CInt, CInt)
defaultMinDimensions = (300, 300)
defaultMaxDimensions = (800, 800)

defaultTBarHeight, defaultBSize :: CInt
defaultTBarHeight = 10
defaultBSize = 10

defaultBColor, defaultTBarColor :: Color
defaultBColor = white
defaultTBarColor = blue
