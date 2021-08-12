{-# LANGUAGE OverloadedStrings #-}

import Draw
import DataTypes

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
  Split main file
-}

white :: Color
white = SDL.V4 255 255 255 0

blue :: Color
blue = SDL.V4 0 0 255 0

black :: Color
black = SDL.V4 0 0 0 0

green :: Color
green = SDL.V4 32 194 14 0

emptyGUI :: GUI
emptyGUI = GUI { windows = [], lastWindowID = 0 }

maxDimension :: Int
maxDimension = 32768

windowSafetyCheck :: Window -> Window
windowSafetyCheck w = let (Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 dx dy)) = dimensions w
                          tBarHeight = titleBarHeight w in
                        if tBarHeight > dy
                        then error "Window failed safety checks"
                        else w

unique :: (Eq a) => [a] -> Bool
unique [] = True
unique (x:xs) = if x `elem` xs
                then False
                else unique xs

guiSafetyCheck :: GUI -> GUI
guiSafetyCheck gui = let guiWindows = windows gui
                         newGUIWindows = map windowSafetyCheck guiWindows
                         doubleID = unique $ map windowID guiWindows in
                       if doubleID
                       then error "GUI Contains doubleID"
                       else gui

createWindow :: ID -> Rectangle CInt -> CInt -> Color ->
                Color -> [(Component, Rectangle CInt, Window -> Window)] -> Window
createWindow wID dmensions tBarHeight bColor tBarColor comps =
  let newWindow = windowSafetyCheck $ Window { windowID = wID, dimensions = dmensions,
                                         beingDragged = False, borderColor = bColor, titleBarFillColor = tBarColor,
                                         titleBarHeight = tBarHeight, components = comps} in
    newWindow

{- This function is meant to be called whenever we need to modify an Window
   eg. modifyWindow $ oldWindow { tBarHeight = 20 }                     -}
modifyWindow :: Window -> Window
modifyWindow = windowSafetyCheck

{- Takes a window and returns a GUI containing that window and an updated ID record -}
addWindowToGUI :: Window -> GUI -> GUI
addWindowToGUI w gui = let newWindowsList = mappend [w] $ windows gui
                           newID = lastWindowID gui + 1 in
                      guiSafetyCheck $ gui { windows = newWindowsList, lastWindowID = newID }

{- Creates a window and immediatly places it into a GUI -}
createGUIWindow :: Rectangle CInt -> CInt -> Color -> Color -> [(Component, Rectangle CInt, Window -> Window)] -> GUI -> GUI
createGUIWindow dmensions tBarHeight bColor tBarColor comps oldGUI = let newWindow = createWindow (lastWindowID oldGUI) dmensions tBarHeight
                                                                                     bColor tBarColor comps in
                                                                      addWindowToGUI newWindow oldGUI

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

clickInsideGUI :: SDL.Point SDL.V2 CInt -> [Window] -> Maybe Window
clickInsideGUI click = getFirst (\x -> dimensions x `insideRectangle` click)

setDragging :: Window -> GUI -> GUI
setDragging oldWindow = updateGUI (modifyWindow $ oldWindow {beingDragged = True})

-- Whether there is a window currently being dragged
windowBeingDragged :: [Window] -> Bool
windowBeingDragged = any beingDragged

getDraggedWindow :: [Window] -> Maybe Window
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
updateGUI newWindow gui = let oldWindows = windows gui in
                            gui {windows = go oldWindows} where
  go [] = []
  go (window:rest) = if windowID window == windowID newWindow
                     then newWindow : rest
                     else window : go rest

moveRectangle :: (CInt, CInt) -> Rectangle CInt -> Rectangle CInt
moveRectangle (dx, dy) (Rectangle (SDL.P (SDL.V2 x y)) botCorner) = Rectangle (SDL.P (SDL.V2 (x + dx) (y + dy))) botCorner

updateDraggedWindow :: SDL.V2 CInt -> Window -> Window
updateDraggedWindow (SDL.V2 dx dy) draggedWindow = let (Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 width height)) = dimensions draggedWindow
                                                       oldComponents = components draggedWindow in
                                    modifyWindow $ draggedWindow {dimensions = (Rectangle (SDL.P (SDL.V2 (x + dx) (y + dy)))
                                                                               (SDL.V2 width height)),
                                                                  components = fmap (\(comp, rect, f) -> (comp, moveRectangle (dx, dy) rect, f))
                                                                               oldComponents}

guiHandleClick :: SDL.InputMotion -> SDL.MouseButton -> SDL.Point SDL.V2 CInt -> GUI -> GUI
guiHandleClick motion button clickCoords oldGUI | motion == SDL.Pressed
                                                  && button == SDL.ButtonLeft =
                                                          case clickInsideGUI clickCoords guiWindows of
                                                            Nothing -> oldGUI
                                                            Just w -> setDragging w oldGUI
                                                | motion == SDL.Released
                                                  && button == SDL.ButtonLeft
                                                  && windowBeingDragged guiWindows =
                                                          let draggedWindow = fromJust $ getDraggedWindow guiWindows in
                                                            updateGUI (modifyWindow $ draggedWindow { beingDragged = False }) oldGUI
                                                | otherwise = oldGUI
                                            where guiWindows = windows oldGUI

guiHandleMotion :: SDL.V2 CInt -> GUI -> GUI
guiHandleMotion motion oldGUI = if windowBeingDragged guiWindows
                                then updateGUI (updateDraggedWindow motion (fromJust $ getDraggedWindow guiWindows)) oldGUI
                                else oldGUI
                              where guiWindows = windows oldGUI

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
                            then gotSomething $ clickInsideGUI (fmap fromIntegral coords) (windows gui)
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
  let startGUI = createGUIWindow (Rectangle (SDL.P (SDL.V2 0 0)) (SDL.V2 500 500))
                                              10 white blue [] emptyGUI

      newGUI = updateGUI (windowAddComponent (head $ windows startGUI) (Rectangle (SDL.P (SDL.V2 5 5)) (SDL.V2 490 440)) text id) startGUI

  loop renderer font newGUI
  SDL.destroyWindow window
