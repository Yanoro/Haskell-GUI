module WindowUtils where

import DataTypes

import qualified SDL
import Foreign.C.Types
import SDL.Video.Renderer
import Data.Maybe

emptyGUI :: GUI
emptyGUI = GUI { windows = [], lastWindowID = 0 }

maxDimension :: Int
maxDimension = 32768

windowSafetyCheck :: Window -> Window
windowSafetyCheck w = let (SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 dx dy)) = dimensions w
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
                         doubleID = not $ unique $ map windowID guiWindows in
                       if doubleID
                       then error "GUI Contains doubleID"
                       else gui

createWindow :: ID -> WType -> SDL.Rectangle CInt -> CInt -> Color ->
                Color -> [(Component, SDL.Rectangle CInt, Window -> Window)] -> Window
createWindow wID wType dmensions tBarHeight bColor tBarColor comps =
  let newWindow = windowSafetyCheck $ Window { windowID = wID, windowType = wType, dimensions = dmensions,
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
createGUIWindow :: WType -> SDL.Rectangle CInt -> CInt -> Color -> Color -> [(Component, SDL.Rectangle CInt, Window -> Window)] -> GUI -> GUI
createGUIWindow wType dmensions tBarHeight bColor tBarColor comps oldGUI = let newWindow = createWindow (lastWindowID oldGUI) wType
                                                                                           dmensions tBarHeight bColor tBarColor comps in
                                                                      addWindowToGUI newWindow oldGUI

splitSurface :: SDL.Surface -> CInt -> CInt -> IO [SDL.Surface]
splitSurface srcSurf width height = mapM (\(x, dx) -> do
                                             let srcRect = SDL.Rectangle (SDL.P (SDL.V2 x 0)) (SDL.V2 dx height)
                                             newSurface <- createRGBSurface (SDL.V2 dx height) RGB444
                                             print (x, dx)
                                             _ <- SDL.surfaceBlit srcSurf (Just srcRect) newSurface Nothing
                                             return newSurface) $ go 0 where
  go :: CInt -> [(CInt, CInt)]
  go x | x >= width = []
       | otherwise = (x, horizSize) : go (x + horizSize)
         where horizSize = min (width - x) (fromIntegral maxDimension)

getFirst :: (a -> Bool) -> [a] -> Maybe a
getFirst _ [] = Nothing
getFirst fun (x:xs) = if fun x
                      then Just x
                      else getFirst fun xs

insideRectangle :: SDL.Rectangle CInt -> SDL.Point SDL.V2 CInt -> Bool
insideRectangle (SDL.Rectangle (SDL.P (SDL.V2 xStart yStart)) (SDL.V2 width height)) (SDL.P (SDL.V2 x y)) =
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

-- Get the drawable surface of a window
getDrawingRect :: Window -> SDL.Rectangle CInt
getDrawingRect w = let (SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 dx dy)) = dimensions w
                       tBarH = titleBarHeight w in
                     SDL.Rectangle (SDL.P (SDL.V2 x (y + tBarH))) (SDL.V2 dx (dy - tBarH))

{- Add a component to a GUI. Input rectangle must be relative -}
windowAddComponent :: Window -> SDL.Rectangle CInt -> Component -> (Window -> Window) -> Window
windowAddComponent w rect comp answerFunction =
  let (SDL.Rectangle (SDL.P (SDL.V2 x y)) _ ) = getDrawingRect w
      (SDL.Rectangle (SDL.P (SDL.V2 x' y')) (SDL.V2 dx' dy')) = rect

      newComponents = mappend (components w)
                              [(comp, SDL.Rectangle (SDL.P (SDL.V2 (x' + x) (y' + y))) (SDL.V2 dx' dy'), answerFunction)] in
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

moveRectangle :: (CInt, CInt) -> SDL.Rectangle CInt -> SDL.Rectangle CInt
moveRectangle (dx, dy) (SDL.Rectangle (SDL.P (SDL.V2 x y)) botCorner) = SDL.Rectangle (SDL.P (SDL.V2 (x + dx) (y + dy))) botCorner

updateDraggedWindow :: SDL.V2 CInt -> Window -> Window
updateDraggedWindow (SDL.V2 dx dy) draggedWindow = let (SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 width height)) = dimensions draggedWindow
                                                       oldComponents = components draggedWindow in
                                    modifyWindow $ draggedWindow {dimensions = (SDL.Rectangle (SDL.P (SDL.V2 (x + dx) (y + dy)))
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
