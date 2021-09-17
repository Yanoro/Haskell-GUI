{-# LANGUAGE InstanceSigs #-}
module GUIDataTypes where

import qualified SDL
import qualified Data.Text as T
import Foreign.C.Types

import Data.Word (Word8)

data WindowType = Default

-- TODO: Change Paragraph to Text
data HTML = EmptySpace | Paragraph String Color FontSize | Break CInt | Img FilePath | HButton | HTMLDOC [HTML] deriving (Show, Eq)

data ClickedBorder = LeftBorder | TopBorder | RightBorder | BottomBorder | NoBorder deriving (Show, Eq)

data WType = HTMLWindow ([HTML], [[SDL.Texture]], (String, [HTMLVar]))
           | StandardWindow

{- The reason that we need a [[SDL.Texture]] is that we need to create multiple
   textures for a single paragraph so we can format it correctly, and since we
   need multiple paragraphs, we need [[SDL.Texture]]

   A StandardWindow is a window that is only capable of having drawings inside of it
   i.e no reading html capabilities -}
data Window = Window {
  windowName :: String,
  windowType :: WType,
  dimensions :: SDL.Rectangle CInt,
  minDimensions :: (CInt, CInt),
  maxDimensions :: (CInt, CInt),
  scrollingOffset :: CInt,
  borderSize :: CInt,
  beingDragged :: Bool,
  beingExpanded :: (Bool, ClickedBorder),
  focused :: Bool,
  borderColor :: Color,
  backgroundColor :: Color
}

--data GUITrans = GUITransform (GUI -> GUI)

type GUI = [Window]

type RenderTree = [RenderTreeNode]
type RenderTreeNode = (SDL.Rectangle CInt, HTML)

type WindowName = String
type Color = SDL.V4 Word8

type FontSize = CInt
type HTMLVar = (String, String)
