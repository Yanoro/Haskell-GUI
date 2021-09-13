module WindowUtils where

import GUIDataTypes
import GUIParser
import GUIConstants

import qualified SDL
import qualified SDL.Font
import qualified Data.Text as T
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

genRenderTree :: SDL.Rectangle CInt -> [HTML] -> [SDL.Rectangle CInt]
genRenderTree drawRect htmlDOC = snd $ foldl (\(oldDrawRect, prevRenderNodes) html ->
                                                let (newDrawRect, newRenderNode) = genRenderNode oldDrawRect html in
                                                  (newDrawRect, mappend prevRenderNodes [newRenderNode])) (drawRect, []) htmlDOC

genRenderNode :: SDL.Rectangle CInt -> HTML -> (SDL.Rectangle CInt, SDL.Rectangle CInt)
genRenderNode (SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 dx dy)) (Paragraph p _ fontSize) =
    let lineHSize = div dx (fromIntegral fontSize)
        amountOfLines = div (length p) (fromIntegral lineHSize)
        lines = fromIntegral $ amountOfLines * fromIntegral fontSize
        paragraphSize = if lines < 1 then fontSize else lines
        paragraphDistance = 20
        drawRect = SDL.Rectangle (SDL.P (SDL.V2 x (y + paragraphSize + paragraphDistance)))
                    (SDL.V2 dx (dy - paragraphSize - borderDistance))
        renderNode = SDL.Rectangle (SDL.P (SDL.V2 (x + borderDistance) (y + borderDistance)))
                    (SDL.V2 (dx - 2 * borderDistance) paragraphSize) in
      (drawRect, renderNode)
genRenderNode (SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 dx dy)) (Break breakSize) =
  let drawRect = SDL.Rectangle (SDL.P (SDL.V2 x (y + borderDistance + breakSize)))
                                (SDL.V2 dx (dy - breakSize))
      renderNode = SDL.Rectangle (SDL.P (SDL.V2 (x + borderDistance) (y + borderDistance)))
                                        (SDL.V2 (dx - 2 * borderDistance) breakSize) in
    (drawRect, renderNode)

findWindowByID :: ID -> GUI -> Maybe Window
findWindowByID targetID gui = getFirst (\w -> windowID w == targetID) $ windows gui

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
      (minWidth, minHeight) = miDimensions
      (maxWidth, maxHeight) = maDimensions
      (SDL.Rectangle _ (SDL.V2 width height)) = dmensions
      bSize = fromMaybe defaultBSize maybeBSize
      bColor = fromMaybe defaultBColor maybeBColor
      newWindow = windowSafetyCheck $ Window { windowID = wID, windowType = wType, dimensions = dmensions, minDimensions = miDimensions,
                                               maxDimensions = maDimensions, borderSize = bSize, beingDragged = False, scrollingOffset = 0,
                                               beingExpanded = (False, NoBorder), borderColor = bColor} in
    if width < minWidth || width > maxWidth || height < minHeight || height > maxHeight
    then error "[!] Invalid dimensions for window"
    else newWindow

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

-- Tells wether the second rectangle is inside the first
-- !! Assumes that the first point of the Rectangle is the left upper corner for both of them !!
rectInsideRect :: SDL.Rectangle CInt -> SDL.Rectangle CInt -> Bool
rectInsideRect rect (SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 dx dy)) =
    pointInsideRectangle rect (SDL.P (SDL.V2 x y)) && pointInsideRectangle rect (SDL.P (SDL.V2 (x + dx) (y + dy)))

-- !! This assumes that the first point is the upper left corner !!
pointInsideRectangle :: SDL.Rectangle CInt -> SDL.Point SDL.V2 CInt -> Bool
pointInsideRectangle (SDL.Rectangle (SDL.P (SDL.V2 xStart yStart)) (SDL.V2 width height)) (SDL.P (SDL.V2 x y)) =
  not $ x < xStart || y < yStart || x > xStart + width || y > yStart + height

--TODO: Change name to point inside GUI
clickInsideGUI :: SDL.Point SDL.V2 CInt -> [Window] -> Maybe Window
clickInsideGUI click = getFirst (\x -> dimensions x `pointInsideRectangle` click)

clickInsideBorder :: SDL.Point SDL.V2 CInt -> [Window] -> Maybe Window
clickInsideBorder _ [] = Nothing
clickInsideBorder click (window:rest) = let borders = getRectBorders (borderSize window) (dimensions window) in
                                          if any (`pointInsideRectangle` click) borders
                                          then Just window
                                          else clickInsideBorder click rest

getRectBorders :: CInt -> SDL.Rectangle CInt -> [SDL.Rectangle CInt]
getRectBorders bSize (SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 dx dy)) =
  let leftBorder   = SDL.Rectangle (SDL.P (SDL.V2 (x - bSize) y)) (SDL.V2 bSize dy)
      topBorder    = SDL.Rectangle (SDL.P (SDL.V2 (x - bSize) (y - bSize))) (SDL.V2 (dx + 2 * bSize) bSize)
      rightBorder  = SDL.Rectangle (SDL.P (SDL.V2 (x + dx) y)) (SDL.V2 bSize dy)
      bottomBorder = SDL.Rectangle (SDL.P (SDL.V2 (x - bSize) (y + dy))) (SDL.V2 (dx + 2 * bSize) bSize) in
    [leftBorder, topBorder, rightBorder, bottomBorder]

setDragging :: Window -> GUI -> GUI
setDragging oldWindow = updateGUI (modifyWindow $ oldWindow {beingDragged = True})

unsetDragging :: Window -> GUI -> GUI
unsetDragging oldWindow = updateGUI (modifyWindow $ oldWindow {beingDragged = False})

setExpansion :: SDL.Point SDL.V2 CInt -> Window -> GUI -> GUI
setExpansion click oldWindow =
  let borders = getRectBorders (borderSize oldWindow) (dimensions oldWindow)
      (SDL.Rectangle (SDL.P (SDL.V2 x' y')) _) = fromJust $ getFirst (`pointInsideRectangle` click) borders
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
      (width, height) = if clickBorder == RightBorder || clickBorder == BottomBorder
                       then (dx + mx, dy + my)
                       else (dx - mx, dy - my)

      (startX, width') | width > maxWidth = (x, maxWidth)
                       | width < minWidth = (x, minWidth)
                       | otherwise = (x + mx, width)
      (startY, height') | height > maxHeight = (y, maxHeight)
                        | height < minHeight = (y, minHeight)
                        | otherwise = (y + my, height)
                        -- The left upper corner moves when the left border or top border are expanded
      newDimensions = if clickBorder == LeftBorder || clickBorder == TopBorder
                      then SDL.Rectangle (SDL.P (SDL.V2 startX startY)) (SDL.V2 width' height')
                      else SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 width' height') in
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

getMaxOffset :: Window -> CInt
getMaxOffset w = let (HTMLWindow (html, _, _)) = windowType w
                     renderTree = genRenderTree (dimensions w) html
                     (SDL.Rectangle (SDL.P (SDL.V2 _ y)) _) = dimensions w
                     (SDL.Rectangle (SDL.P (SDL.V2 _ y')) _) = last renderTree
                     maxOffset = y' - y - relativeMaxOffset in
                   maxOffset


guiHandleScrolling :: SDL.V2 CInt -> GUI -> IO GUI
guiHandleScrolling (SDL.V2 _ dy) oldGUI = do
  let oldWindows = windows oldGUI
      dy' = -dy -- The default scrolling direction is a little weird so we need to flip it

  mouseCoords <- SDL.getAbsoluteMouseLocation
  case clickInsideGUI mouseCoords oldWindows of
    Nothing -> return oldGUI
    Just w -> let maxOffset = getMaxOffset w
                  prevOffset = scrollingOffset w
                  newScrollingOffset = prevOffset + dy' * scrollingScale in
                do
                print newScrollingOffset
                print maxOffset
                if newScrollingOffset < minOffset || newScrollingOffset > maxOffset
                then return oldGUI
                else return $ updateGUI (modifyWindow $ w {scrollingOffset = prevOffset + dy' * scrollingScale}) oldGUI

guiHandleEvent :: SDL.EventPayload -> GUI -> IO GUI
guiHandleEvent (SDL.MouseButtonEvent (SDL.MouseButtonEventData _ motion _ button _ clickCoords)) oldGUI =
                                                return $ guiHandleClick motion button (fromIntegral <$> clickCoords) oldGUI
guiHandleEvent (SDL.MouseMotionEvent (SDL.MouseMotionEventData _ _ _ _ motion)) oldGUI =
                                                return $ guiHandleMotion (fromIntegral <$> motion) oldGUI
guiHandleEvent (SDL.MouseWheelEvent (SDL.MouseWheelEventData _ _ scrolling _)) oldGUI =
                                                                guiHandleScrolling (fromIntegral <$> scrolling) oldGUI
guiHandleEvent _ oldGUI = return oldGUI

lookupReplace :: Eq a => [(a, b)] -> a -> b -> Maybe [(a, b)]
lookupReplace [] _ _ = Nothing
lookupReplace table key replacement = go table (Just []) where
  go [] _ = Nothing
  go ((currentKey, currentItem):rest) iter =
    if key == currentKey
    then (++ ((key, replacement) :rest)) <$> iter
    else go rest ((++ [(currentKey, currentItem)]) <$> iter)

-- Generates for each html tag their required textures, in this case only paragraph needs it.
-- It's important that at the stage where the html gets drawn, the list of html tags given is the
-- same as the one given given in this function, otherwise html tags are going to be drawn with other
-- tag's textures
genHTMLTextures :: SDL.Renderer -> SDL.Font.Font -> [HTML] -> IO [[SDL.Texture]]
genHTMLTextures render font htmlDOC = do
  texts <- mapM go htmlDOC
  return $ filter ([] /=) texts where
  go (Paragraph p color _) = do
    let p' = T.pack p
    surf <- SDL.Font.blended font color p'
    (SDL.V2 width height) <- SDL.surfaceDimensions surf
    surfaces <- if width > fromIntegral maxDimension then splitSurface surf (fromIntegral width) height else return [surf]
    texts <- mapM (SDL.createTextureFromSurface render) surfaces
    SDL.freeSurface surf
    return texts
  go (Break _) = return []
  go _ = return []

reloadHTMLVar :: SDL.Renderer -> SDL.Font.Font -> Window -> HTMLVar -> GUI -> IO GUI
reloadHTMLVar render font w htmlVar oldGUI =
  do
  let (HTMLWindow (_, _, rawText)) = windowType w

  let htmlText' = loadVariables htmlVar rawText
      newParsedHTML = fst $ head $ runParser parseHTML htmlText'

  newTextures <- genHTMLTextures render font newParsedHTML
  return $ updateGUI (modifyWindow $ w { windowType = HTMLWindow (newParsedHTML, newTextures, htmlText')}) oldGUI

{- Convenience function so we don't forget to load our variables
   Remember that the order that the variables are given DO matter! -}

loadHTMLFile :: FilePath -> [HTMLVar] -> IO String
loadHTMLFile fileName vars = do
  htmlText <- readFile fileName
  return $ go vars htmlText where
  go [] res = res
  go (htmlVar:rest) res = go rest (loadVariables htmlVar res)
