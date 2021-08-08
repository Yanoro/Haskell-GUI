{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL
import qualified SDL.Font
import qualified Data.Text as T

import SDL.Video.Renderer
import Foreign.C.Types
import Control.Monad
import Control.Monad.IO.Class (liftIO, MonadIO)

import Data.Maybe (fromJust)
import Data.Word (Word8)
import Data.Bifunctor (second)

{- TODO:
  Add component safety checks
-}

data WindowType = Default

data Component = Button Color | Text T.Text Color [Texture]

data Window = Window {
  windowID :: ID,
  dimensions :: Rectangle CInt,
  titleBarHeight :: CInt,
  beingDragged :: Bool,
  borderColor :: Color,
  titleBarFillColor :: Color,
  components :: [(Component, Rectangle CInt, Window -> Window)]
}

type ID = Int
type GUI = [Window]
type Color = SDL.V4 Word8

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

fontSize :: CInt
fontSize = 12

maxDimension :: Int
maxDimension = 32768

splitSurface :: SDL.Surface -> CInt -> CInt -> IO [SDL.Surface]
splitSurface srcSurf width height = mapM (\(x, dx) -> do
                                             let srcRect = Rectangle (SDL.P (SDL.V2 x 0)) (SDL.V2 dx height)
                                             newSurface <- createRGBSurface (SDL.V2 dx height) RGB444
                                             print (x, dx)
                                             _ <- SDL.surfaceBlit srcSurf (Just srcRect) newSurface Nothing
                                             return newSurface) $ go 0 where
  go :: CInt -> [(CInt, CInt)]
  go x | x >= width = []
       | otherwise = (x, horizSize) : go (x + horizSize)
         where horizSize = min (width - x) (fromIntegral maxDimension)

createTextComponent :: SDL.Renderer -> T.Text -> Color -> SDL.Font.Font -> IO Component
createTextComponent render msg color font = do
  surf <- SDL.Font.blended font color msg
  (SDL.V2 width height) <- surfaceDimensions surf
  print width
  print maxDimension
  surfaces <- if width > fromIntegral maxDimension then splitSurface surf (fromIntegral width) height else return [surf]
  texts <- mapM (createTextureFromSurface render) surfaces
  freeSurface surf
  return $ Text msg color texts


srcDestRects :: Int -> [(SDL.Texture, SDL.TextureInfo)] -> Rectangle CInt -> [((SDL.Texture, Rectangle CInt), Rectangle CInt)]
srcDestRects msgLength texts rect = let dRecs = destRects msgLength rect
                                        sRecs = srcRects msgLength texts dRecs in
                                      zip sRecs dRecs

srcRects :: Int -> [(SDL.Texture, SDL.TextureInfo)] -> [Rectangle CInt] -> [(SDL.Texture, Rectangle CInt)]
srcRects msgLen = go 0 where
  go x' texts destRects  | null destRects = [(text, Rectangle (SDL.P (SDL.V2 x' 0)) (SDL.V2 (x' - x) y))]
                         | x' >= x = go 0 (tail texts) destRects
                         | otherwise = (text, Rectangle (SDL.P (SDL.V2 x' 0)) (SDL.V2 textLineSize y)) : go nextX
                                                                                                        texts
                                                                                                        (tail destRects)
    where nextX = x' + textLineSize
          (Rectangle _ (SDL.V2 dx _)) = head destRects
          lineSize = div dx fontSize
          hdTexts = head texts
          (text, textInfo) = hdTexts
          (x, y) = (textureWidth textInfo, textureHeight textInfo)
          charSize = div x (fromIntegral msgLen)
          textLineSize = charSize * lineSize

destRects :: Int -> Rectangle CInt -> [Rectangle CInt]
destRects msgLen (Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 dx dy)) =
  go 0 msgLen where
  lineSize = div dx fontSize
  go :: CInt -> Int -> [Rectangle CInt]
  go n msgRem | msgRem == 0 = []
              | msgRem <= fromIntegral lineSize = [Rectangle (SDL.P (SDL.V2 x (y + 12 * n)))
                                                   (SDL.V2 (fromIntegral $ 12 * (msgRem + 1)) fontSize)]
              | otherwise = Rectangle (SDL.P (SDL.V2 x (y + 12 * n))) (SDL.V2 dx fontSize)  : go (n + 1)
                                                                                              (msgRem - fromIntegral lineSize)

getDrawingRect :: Window -> Rectangle CInt
getDrawingRect w = let (Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 dx dy)) = dimensions w
                       tBarH = titleBarHeight w in
                     Rectangle (SDL.P (SDL.V2 (x + 1) (y + tBarH))) (SDL.V2 (dx - 2) (dy - tBarH - 1))

drawComponent :: SDL.Renderer -> SDL.Font.Font -> (Component, Rectangle CInt, Window -> Window) -> IO ()
drawComponent render _ (Button color, rect, _) = do
  rendererDrawColor render SDL.$= color
  drawRect render (Just rect)

drawComponent render _ (Text msg color texts, rect, _) = do
  rendererDrawColor render SDL.$= color
  infoTexts <- mapM (\text -> do
                        textInfo <- queryTexture text
                        return (text, textInfo)) texts

  let sdRects = srcDestRects (T.length msg) infoTexts rect
      dRects = map snd sdRects

  mapM_ (\((text, s), d) -> do
            copy render text (Just s) (Just d)) sdRects

  mapM_ (\d -> do
            rendererDrawColor render SDL.$= red
            drawRect render (Just d)) dRects

  drawRect render (Just rect)

drawWindow :: SDL.Renderer -> SDL.Font.Font -> Window -> IO ()
drawWindow render font w = do
  let outerRect = dimensions w
      bColor = borderColor w
      tColor = titleBarFillColor w
      (Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 dx _)) = outerRect
      relTBarH = titleBarHeight w
      absTBarH = relTBarH + y

      -- We need those additions and subtractions so lines don't go over where they should go
      (titleBarStart, titleBarEnd) = (SDL.P (SDL.V2 x absTBarH), SDL.P (SDL.V2 (x + dx - 1) absTBarH))
      titleBarColorRect = Rectangle (SDL.P (SDL.V2 (x + 1) (y + 1))) (SDL.V2 (dx - 2) (relTBarH - 1))

      comps = components w

  rendererDrawColor render SDL.$= bColor
  drawRect render (Just outerRect) --Draw Main Frame

  drawLine render titleBarStart titleBarEnd -- Draw Title Bar line

  rendererDrawColor render SDL.$= tColor
  fillRect render (Just titleBarColorRect) -- Fill tile bar with color

  mapM_ (drawComponent render font) comps

drawGUI :: SDL.Renderer -> SDL.Font.Font -> GUI -> IO ()
drawGUI render font = mapM_ (drawWindow render font)

getFirst :: (a -> Bool) -> [a] -> Maybe a
getFirst _ [] = Nothing
getFirst fun (x:xs) = if fun x
                      then Just x
                      else getFirst fun xs

topLeft :: Rectangle CInt -> SDL.V2 CInt
topLeft (Rectangle (SDL.P res) _) = res

insideRectangle :: Rectangle CInt -> SDL.Point SDL.V2 CInt -> Bool
insideRectangle (Rectangle (SDL.P (SDL.V2 xStart yStart)) (SDL.V2 width height)) (SDL.P (SDL.V2 x y)) =
  not $ x < xStart || y < yStart || x > xStart + width || y > yStart + height

clickInsideGUI :: SDL.Point SDL.V2 CInt -> GUI -> Maybe Window
clickInsideGUI click = getFirst (\x -> dimensions x `insideRectangle` click)

setDragging :: Window -> GUI -> GUI
setDragging oldWindow = updateGUI (oldWindow {beingDragged = True})

-- Whether there is a window currently being dragged
windowBeingDragged :: GUI -> Bool
windowBeingDragged = any beingDragged

getDraggedWindow :: GUI -> Maybe Window
getDraggedWindow = getFirst beingDragged

gotSomething :: Maybe a -> Bool
gotSomething (Just _) = True
gotSomething Nothing = False

{- Add a component to a GUI. Input rectangle must be relative -}
windowAddComponent :: Window -> Rectangle CInt -> Component -> (Window -> Window) -> Window
windowAddComponent w rect comp answerFunction =
  let (Rectangle (SDL.P (SDL.V2 x y)) _ ) = getDrawingRect w
      (Rectangle (SDL.P (SDL.V2 x' y')) (SDL.V2 dx' dy')) = rect

      newComponents = mappend (components w)
                              [(comp, Rectangle (SDL.P (SDL.V2 (x' + x) (y' + y))) (SDL.V2 dx' dy'), answerFunction)] in
    (w {components = newComponents})

{- The windowID of the newWindow has to be the same as the old one.
   This way we don't need to keep passing a targetID to the function just to "update" the gui -}
updateGUI :: Window -> GUI -> GUI
updateGUI _ [] = []
updateGUI newWindow (window:rest) = if windowID window == windowID newWindow
                                       then newWindow : rest
                                       else window : updateGUI newWindow rest

moveRectangle :: (CInt, CInt) -> Rectangle CInt -> Rectangle CInt
moveRectangle (dx, dy) (Rectangle (SDL.P (SDL.V2 x y)) botCorner) = Rectangle (SDL.P (SDL.V2 (x + dx) (y + dy))) botCorner

updateDraggedWindow :: SDL.V2 CInt -> Window -> Window
updateDraggedWindow (SDL.V2 dx dy) draggedWindow = let (Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 width height)) = dimensions draggedWindow
                                                       oldComponents = components draggedWindow in
                                           draggedWindow {dimensions = (Rectangle (SDL.P (SDL.V2 (x + dx) (y + dy)))
                                                                                         (SDL.V2 width height)),
                                                          components = fmap (\(comp, rect, f) -> (comp, moveRectangle (dx, dy) rect, f))
                                                                            oldComponents}


guiHandleClick :: SDL.InputMotion -> SDL.MouseButton -> SDL.Point SDL.V2 CInt -> GUI -> GUI
guiHandleClick motion button clickCoords oldGUI | motion == SDL.Pressed
                                                  && button == SDL.ButtonLeft =
                                                          case clickInsideGUI clickCoords oldGUI of
                                                            Nothing -> oldGUI
                                                            Just w -> setDragging w oldGUI
                                                | motion == SDL.Released
                                                  && button == SDL.ButtonLeft
                                                  && windowBeingDragged oldGUI =
                                                          let draggedWindow = fromJust $ getDraggedWindow oldGUI in
                                                            updateGUI (draggedWindow { beingDragged = False }) oldGUI
                                                | otherwise = oldGUI

guiHandleMotion :: SDL.V2 CInt -> GUI -> GUI
guiHandleMotion motion oldGUI = if windowBeingDragged oldGUI
                                then updateGUI (updateDraggedWindow motion (fromJust $ getDraggedWindow oldGUI)) oldGUI
                                else oldGUI

guiHandleEvent :: SDL.EventPayload -> GUI -> GUI
guiHandleEvent (SDL.MouseButtonEvent (SDL.MouseButtonEventData _ motion _ button _ clickCoords)) oldGUI =
                                                                guiHandleClick motion button (fromIntegral <$> clickCoords) oldGUI
guiHandleEvent (SDL.MouseMotionEvent (SDL.MouseMotionEventData _ _ _ _ motion)) oldGUI =
                                                                guiHandleMotion (fromIntegral <$> motion) oldGUI
guiHandleEvent _ oldGUI = oldGUI

loop :: SDL.Renderer -> SDL.Font.Font -> GUI -> IO ()
loop render font gui = do
  events <- SDL.pollEvents
  let quitKey event =
        case SDL.eventPayload event of
          SDL.KeyboardEvent keyboardEvent ->
            SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
            SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeEscape
          _ -> False
      handleMouse event =
        case SDL.eventPayload event of
          SDL.MouseButtonEvent (SDL.MouseButtonEventData _ motion _ button _ coords)->
                            if motion == SDL.Pressed && button == SDL.ButtonLeft
                            then gotSomething $ clickInsideGUI (fmap fromIntegral coords) gui
                            else False

          _ -> False
      quitKeyPressed = any quitKey events
      newGUI = foldr (guiHandleEvent . SDL.eventPayload) gui events


  if any handleMouse events then
    print $ any handleMouse events
  else return ()

  SDL.rendererDrawColor render SDL.$= SDL.V4 0 0 0 0

  SDL.clear render
  drawGUI render font gui
  SDL.present render

  unless quitKeyPressed $ loop render font newGUI

main :: IO ()
main = do
  SDL.initializeAll
  SDL.Font.initialize

  font <- SDL.Font.loadIndex "../assets/font.otf" 180 0

  window <- SDL.createWindow "Roguelike" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) defaultRenderer
  text <- createTextComponent renderer "He sat staring at the person in the train stopped at the station going in the opposite direction. She sat staring ahead, never noticing that she was being watched. Both trains began to move and he knew that in another timeline or in another universe, they had been happy together. At las, he was not a timetravelerr" green font

  SDL.Font.setHinting font SDL.Font.Mono
  size <- SDL.Font.getHinting font
  print size
  let startGUI = [Window { windowID = 1, dimensions = Rectangle (SDL.P (SDL.V2 0 0)) (SDL.V2 500 500),
                           beingDragged = False, borderColor = white, titleBarFillColor = blue,
                           titleBarHeight = 10, components = []}]
      newGUI = updateGUI (windowAddComponent (head startGUI) (Rectangle (SDL.P (SDL.V2 5 5)) (SDL.V2 490 440)) text id) startGUI

  loop renderer font newGUI
  SDL.destroyWindow window
