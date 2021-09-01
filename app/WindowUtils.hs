module WindowUtils where

import DataTypes
import Parser
import Constants

import qualified SDL
import Foreign.C.Types
import SDL.Video.Renderer
import Data.Maybe

emptyGUI :: GUI
emptyGUI = GUI { windows = [], lastWindowID = 0 }

maxDimension :: Int
maxDimension = 32768

-- TODO: Actually make this be useful :D
windowSafetyCheck :: Window -> Window
windowSafetyCheck w = w

unique :: (Eq a) => [a] -> Bool
unique [] = True
unique (x:xs) = notElem x xs && unique xs

guiSafetyCheck :: GUI -> GUI
guiSafetyCheck gui = let guiWindows = windows gui
                         newGUIWindows = map windowSafetyCheck guiWindows
                         doubleID = not $ unique $ map windowID guiWindows in
                       if doubleID
                       then error "GUI Contains doubleID"
                       else gui

createWindow :: ID -> WType -> Maybe (SDL.Rectangle CInt) -> Maybe (CInt, CInt) -> Maybe (CInt, CInt) -> Maybe CInt -> Maybe Color ->
                Window
createWindow wID wType maybeDimensions maybeMinDimensions maybeMaxDimensions maybeBSize maybeBColor =
  let dmensions = fromMaybe defaultDimensions maybeDimensions
      miDimensions = fromMaybe defaultMinDimensions maybeMinDimensions
      maDimensions = fromMaybe defaultMaxDimensions maybeMaxDimensions
      bSize = fromMaybe defaultBSize maybeBSize
      bColor = fromMaybe defaultBColor maybeBColor

      newWindow = windowSafetyCheck $ Window { windowID = wID, windowType = wType, dimensions = dmensions, minDimensions = miDimensions,
                                               maxDimensions = maDimensions, borderSize = bSize, beingDragged = False,
                                               beingExpanded = (False, NoBorder), borderColor = bColor} in

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
createGUIWindow :: WType -> Maybe (SDL.Rectangle CInt) -> Maybe (CInt, CInt) -> Maybe (CInt, CInt) -> Maybe CInt -> Maybe CInt -> Maybe Color -> Maybe Color
                   -> GUI -> GUI
createGUIWindow wType dmensions minDimensions maxDimensions tBarHeight bSize bColor tBarColor oldGUI =
  let newWindow = createWindow (lastWindowID oldGUI) wType dmensions
                  minDimensions maxDimensions bSize bColor in
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

-- !! This assumes that the first point is the upper left corner !!
insideRectangle :: SDL.Rectangle CInt -> SDL.Point SDL.V2 CInt -> Bool
insideRectangle (SDL.Rectangle (SDL.P (SDL.V2 xStart yStart)) (SDL.V2 width height)) (SDL.P (SDL.V2 x y)) =
  not $ x < xStart || y < yStart || x > xStart + width || y > yStart + height

clickInsideGUI :: SDL.Point SDL.V2 CInt -> [Window] -> Maybe Window
clickInsideGUI click = getFirst (\x -> dimensions x `insideRectangle` click)

clickInsideBorder :: SDL.Point SDL.V2 CInt -> [Window] -> Maybe Window
clickInsideBorder _ [] = Nothing
clickInsideBorder click (window:rest) = let borders = getRectBorders (borderSize window) (dimensions window) in
                                          if any (`insideRectangle` click) borders
                                          then Just window
                                          else clickInsideBorder click rest

getRectBorders :: CInt -> SDL.Rectangle CInt -> [SDL.Rectangle CInt]
getRectBorders bSize (SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 dx dy)) =
  let topBorder    = SDL.Rectangle (SDL.P (SDL.V2 (x - bSize) (y - bSize))) (SDL.V2 (dx + 2 * bSize) bSize)
      bottomBorder = SDL.Rectangle (SDL.P (SDL.V2 (x - bSize) (y + dy))) (SDL.V2 (dx + 2 * bSize) bSize)
      leftBorder   = SDL.Rectangle (SDL.P (SDL.V2 (x - bSize) y)) (SDL.V2 bSize dy)
      rightBorder  = SDL.Rectangle (SDL.P (SDL.V2 (x + dx) y)) (SDL.V2 bSize dy) in
    [leftBorder, topBorder, rightBorder, bottomBorder]

setDragging :: Window -> GUI -> GUI
setDragging oldWindow = updateGUI (modifyWindow $ oldWindow {beingDragged = True})

unsetDragging :: Window -> GUI -> GUI
unsetDragging oldWindow = updateGUI (modifyWindow $ oldWindow {beingDragged = False})

setExpansion :: SDL.Point SDL.V2 CInt -> Window -> GUI -> GUI
setExpansion click oldWindow =
  let borders = getRectBorders (borderSize oldWindow) (dimensions oldWindow)
      (SDL.Rectangle (SDL.P (SDL.V2 x' y')) _) = fromJust $ getFirst (`insideRectangle` click) borders
      rectCornerTable = zip (map (\(SDL.Rectangle (SDL.P (SDL.V2 m n)) _) -> (m, n)) borders) [LeftBorder, TopBorder, RightBorder, BottomBorder]

      clickedBorder = fromMaybe NoBorder $ lookup clickedBorderCoords rectCornerTable
                where clickedBorderCoords = (x', y') in

  updateGUI (modifyWindow $ oldWindow {beingExpanded = (True, clickedBorder)})

unsetExpansion :: Window -> GUI -> GUI
unsetExpansion oldWindow = updateGUI (modifyWindow $ oldWindow {beingExpanded = (False, NoBorder)})

-- Whether there is a window currently being dragged
windowBeingDragged :: [Window] -> Bool
windowBeingDragged = any beingDragged

windowBeingExpanded :: [Window] -> Bool
windowBeingExpanded = any (fst . beingExpanded)

getDraggedWindow :: [Window] -> Maybe Window
getDraggedWindow = getFirst beingDragged

getExpandedWindow :: [Window] -> Maybe Window
getExpandedWindow = getFirst (fst . beingExpanded)

gotSomething :: Maybe a -> Bool
gotSomething (Just _) = True
gotSomething Nothing = False

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex index newItem lst = a ++ (newItem:b) where (a, _:b) = splitAt index lst

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

updateDraggedWindow :: SDL.V2 CInt -> Window -> GUI -> GUI
updateDraggedWindow (SDL.V2 dx dy) draggedWindow oldGUI =
  let (SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 width height)) = dimensions draggedWindow in
      updateGUI (modifyWindow $ draggedWindow {dimensions = SDL.Rectangle (SDL.P (SDL.V2 (x + dx) (y + dy)))
                                                                                 (SDL.V2 width height)}) oldGUI

updateExpandedWindow :: SDL.V2 CInt -> Window -> GUI -> GUI
updateExpandedWindow (SDL.V2 mx my) expandedWindow oldGUI =
  let (_, clickBorder) = beingExpanded expandedWindow
      oldDimensions = dimensions expandedWindow
      (maxWidth, maxHeight) = maxDimensions expandedWindow
      (minWidth, minHeight) = minDimensions expandedWindow
      (SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 dx dy)) = oldDimensions
      dimensions' = case clickBorder of
                        RightBorder  -> SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 (dx + mx) (dy + my))
                        BottomBorder -> SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 (dx + mx) (dy + my)) 
                        LeftBorder   -> SDL.Rectangle (SDL.P (SDL.V2 (x + mx) (y + my))) (SDL.V2 (dx - mx) (dy - my))
                        TopBorder    -> SDL.Rectangle (SDL.P (SDL.V2 (x + mx) (y + my))) (SDL.V2 (dx - mx) (dy - my))
                        NoBorder     -> oldDimensions
      (SDL.Rectangle start (SDL.V2 width height)) = dimensions'
      width' | width > maxWidth = maxWidth
             | width < minWidth = minWidth
             | otherwise = width
      height' | height > maxHeight = maxHeight
              | height < minHeight = minHeight
              | otherwise = height
      newDimensions = SDL.Rectangle start (SDL.V2 width' height') in
    updateGUI (modifyWindow $ expandedWindow {dimensions = newDimensions}) oldGUI

guiHandleClick :: SDL.InputMotion -> SDL.MouseButton -> SDL.Point SDL.V2 CInt -> GUI -> GUI
guiHandleClick motion button clickCoords oldGUI | motion == SDL.Pressed
                                                  && button == SDL.ButtonLeft =
                                                          case clickInsideGUI clickCoords guiWindows of
                                                            Just w -> setDragging w oldGUI
                                                            Nothing -> case clickInsideBorder clickCoords guiWindows of
                                                                         Nothing -> oldGUI
                                                                         Just w -> setExpansion clickCoords w oldGUI
                                                | motion == SDL.Released
                                                  && button == SDL.ButtonLeft
                                                  && windowBeingDragged guiWindows =
                                                          let draggedWindow = fromJust $ getDraggedWindow guiWindows in
                                                            unsetDragging draggedWindow oldGUI
                                                | motion == SDL.Released
                                                  && button == SDL.ButtonLeft
                                                  && windowBeingExpanded guiWindows =
                                                          let expandedWindow = fromJust $ getExpandedWindow guiWindows in
                                                            unsetExpansion expandedWindow oldGUI
                                                | otherwise = oldGUI
                                            where guiWindows = windows oldGUI

guiHandleMotion :: SDL.V2 CInt -> GUI -> GUI
guiHandleMotion motion oldGUI | windowBeingDragged guiWindows = updateDraggedWindow motion (fromJust $ getDraggedWindow guiWindows) oldGUI
                              | windowBeingExpanded guiWindows = updateExpandedWindow motion (fromJust $ getExpandedWindow guiWindows) oldGUI
                              | otherwise = oldGUI
                        where guiWindows = windows oldGUI


guiHandleEvent :: SDL.EventPayload -> GUI -> GUI
guiHandleEvent (SDL.MouseButtonEvent (SDL.MouseButtonEventData _ motion _ button _ clickCoords)) oldGUI =
                                                                guiHandleClick motion button (fromIntegral <$> clickCoords) oldGUI
guiHandleEvent (SDL.MouseMotionEvent (SDL.MouseMotionEventData _ _ _ _ motion)) oldGUI =
                                                                guiHandleMotion (fromIntegral <$> motion) oldGUI
guiHandleEvent _ oldGUI = oldGUI

{- Convenience function so we don't forget to load our variables
   Remember that the order that the variables are given DO matter! -}

loadHTMLFile :: FilePath -> [HTMLVar] -> IO String
loadHTMLFile fileName vars = do
  html <- readFile fileName
  return $ loadVariables vars html
