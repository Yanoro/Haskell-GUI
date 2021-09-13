module GUIDataTypes where

import qualified SDL
import qualified Data.Text as T
import Foreign.C.Types

import Data.Word (Word8)

data WindowType = Default

-- TODO: Change Paragraph to Text
data HTML = EmptySpace | Paragraph String Color FontSize | Break CInt | Img FilePath | HButton | HTMLDOC [HTML] deriving (Show, Eq)

data ClickedBorder = LeftBorder | TopBorder | RightBorder | BottomBorder | NoBorder deriving (Show, Eq)

{- The reason that we need a [[SDL.Texture]] is that we need to create multiple
   textures for a single paragraph so we can format it correctly, and since we
   need multiple paragraphs, we need [[SDL.Texture]] -}

newtype WType = HTMLWindow ([HTML], [[SDL.Texture]], String)

data Window = Window {
  windowID :: ID,
  windowType :: WType,
  dimensions :: SDL.Rectangle CInt,
  minDimensions :: (CInt, CInt),
  maxDimensions :: (CInt, CInt),
  scrollingOffset :: CInt,
  borderSize :: CInt,
  beingDragged :: Bool,
  beingExpanded :: (Bool, ClickedBorder),
  borderColor :: Color
}

data GUI = GUI {
  windows :: [Window],
  lastWindowID :: ID
}

type RenderTree = [RenderTreeNode]
type RenderTreeNode = (SDL.Rectangle CInt, HTML)

type ID = Int
type Color = SDL.V4 Word8

type FontSize = CInt
type HTMLVar = (String, String)
