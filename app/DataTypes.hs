module DataTypes where

import qualified SDL
import qualified Data.Text as T
import Foreign.C.Types

import Data.Word (Word8)

data WindowType = Default

data Component = Button Color | Text T.Text Color [SDL.Texture]

data Window = Window {
  windowID :: ID,
  dimensions :: SDL.Rectangle CInt,
  titleBarHeight :: CInt,
  beingDragged :: Bool,
  borderColor :: Color,
  titleBarFillColor :: Color,
  components :: [(Component, SDL.Rectangle CInt, Window -> Window)]
}

data GUI = GUI {
  windows :: [Window],
  lastWindowID :: ID
}

type ID = Int
type Color = SDL.V4 Word8
