module GUIConstants where

import GUIDataTypes

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

-- Amount to scale the change in the y coordinate when scrolling a window
scrollingScale :: CInt
scrollingScale = 5

{- Since we need to generate a render tree to know the length of the entire window
   the best we can do for the maxOffset is an offset to be used after getting the last
   member of a render tree -}
minOffset, relativeMaxOffset :: CInt
minOffset = -20
relativeMaxOffset = 300

{- The distance between the borders of a window and newly placed content
   e.g Paragraphs, images etc -}
borderDistance :: CInt
borderDistance = 10

{- Default parameters for a window -}

defaultDimensions :: Rectangle CInt
defaultDimensions = Rectangle (SDL.P (SDL.V2 0 0)) (SDL.V2 500 500)

-- First element represents min/max width, the second min/max height
defaultMinDimensions, defaultMaxDimensions :: (CInt, CInt)
defaultMinDimensions = (300, 300)
defaultMaxDimensions = (800, 800)

defaultTBarHeight, defaultBSize :: CInt
defaultTBarHeight = 10
defaultBSize = 10

defaultBColor, defaultBackgroundColor, defaultTBarColor :: Color
defaultBColor = white
defaultBackgroundColor = black
defaultTBarColor = blue
