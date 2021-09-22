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
emptyGUI = []

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

findWindowByName :: WindowName -> GUI -> Maybe Window
findWindowByName targetName gui = getFirst (\w -> windowName w == targetName) gui

unique :: (Eq a) => [a] -> Bool
unique [] = True
unique (x:xs) = notElem x xs && unique xs

-- TODO: Make this do something
guiSafetyCheck :: GUI -> GUI
guiSafetyCheck gui = gui

--TODO: There is a better way to do this, just create a defaultWindow variable and remove all the maybes
createWindow :: WindowName -> WType -> Maybe (SDL.Rectangle CInt) -> Maybe (CInt, CInt) -> Maybe (CInt, CInt) ->
                Maybe CInt -> Maybe Color -> Maybe Color -> Window
createWindow wName wType maybeDimensions maybeMinDimensions maybeMaxDimensions maybeBSize maybeBorderColor maybeBackgroundColor =
  let dmensions = fromMaybe defaultDimensions maybeDimensions
      miDimensions = fromMaybe defaultMinDimensions maybeMinDimensions
      maDimensions = fromMaybe defaultMaxDimensions maybeMaxDimensions
      (minWidth, minHeight) = miDimensions
      (maxWidth, maxHeight) = maDimensions
      (SDL.Rectangle _ (SDL.V2 width height)) = dmensions
      bSize = fromMaybe defaultBSize maybeBSize
      bColor = fromMaybe defaultBColor maybeBorderColor
      backColor = fromMaybe defaultBackgroundColor maybeBackgroundColor
      newWindow = windowSafetyCheck $ Window { windowName = wName, windowType = wType, dimensions = dmensions, minDimensions = miDimensions,
                                               maxDimensions = maDimensions, borderSize = bSize, beingDragged = False, scrollingOffset = 0,
                                               beingExpanded = (False, NoBorder), borderColor = bColor, backgroundColor = backColor,
                                               focused = False, drawFun = Nothing} in
    if width < minWidth || width > maxWidth || height < minHeight || height > maxHeight
    then error "[!] Invalid dimensions for window"
    else newWindow

{- This function is meant to be called whenever we need to modify an Window
   eg. modifyWindow $ oldWindow { tBarHeight = 20 }                     -}
modifyWindow :: Window -> Window
modifyWindow = windowSafetyCheck

addWindowsToGUI :: [Window] -> GUI -> GUI
addWindowsToGUI [] res = res
addWindowsToGUI (w:rest) currentGUI = addWindowsToGUI rest $ addWindowToGUI w currentGUI

{- Takes a window and GUI, returns a GUI containing that window -}
addWindowToGUI :: Window -> GUI -> GUI
addWindowToGUI w gui = let newGUI = mappend gui [w] in
                         guiSafetyCheck newGUI

{- Creates a window and immediatly places it into a GUI -}
createGUIWindow :: WindowName -> WType -> Maybe (SDL.Rectangle CInt) -> Maybe (CInt, CInt) -> Maybe (CInt, CInt) -> Maybe CInt ->
          Maybe CInt -> Maybe Color -> Maybe Color -> Maybe Color -> GUI -> GUI
createGUIWindow windowName wType dmensions minDimensions maxDimensions tBarHeight bSize bColor tBarColor backColor oldGUI =
  let newWindow = createWindow windowName wType dmensions
                  minDimensions maxDimensions bSize bColor backColor in
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

scaleRect :: CInt -> SDL.Rectangle CInt -> SDL.Rectangle CInt
scaleRect scale (SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 dx dy)) = SDL.Rectangle (SDL.P (SDL.V2 (x - scale) (y - scale)))
                                                                                    (SDL.V2 (dx + 2 * scale) (dy + scale))

getBorderHitboxes :: Window -> [SDL.Rectangle CInt]
getBorderHitboxes window = let borders = getRectBorders (borderSize window) (dimensions window)
                               scaling = borderHitboxScaling
                               leftBorder = head borders
                               topBorder = borders !! 1
                               rightBorder = borders !! 2
                               (SDL.Rectangle (SDL.P (SDL.V2 xb yb)) (SDL.V2 dxb dyb)) = borders !! 3
                               getVertHitbox (SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 dx dy)) =
                                 SDL.Rectangle (SDL.P (SDL.V2 (x - scaling) (y - scaling)))
                                                      (SDL.V2 (dx + 2 * scaling) (dy + scaling))
                               getHorizHitbox (SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 dx dy)) =
                                 SDL.Rectangle (SDL.P (SDL.V2 (x - scaling) (y - scaling)))
                                                      (SDL.V2 (dx + 2 * scaling) (dy + scaling))
                               newBottomBorder = SDL.Rectangle (SDL.P (SDL.V2 (xb - scaling) yb))
                                                                      (SDL.V2 (dxb + 2 * scaling) (dyb + scaling)) in
                             [getVertHitbox leftBorder, getHorizHitbox topBorder, getVertHitbox rightBorder, newBottomBorder]

-- Makes sure that the priority window is draw last so it doesn't get 'buried' by other windows
sortGUI :: GUI -> GUI
sortGUI oldGUI = if any focused oldGUI
                 then let priorityWindow = fromJust $ getFirst focused oldGUI
                          rest = filter (not . focused) oldGUI in
                        mappend rest [priorityWindow]
                 else oldGUI

clickInsideBorder :: SDL.Point SDL.V2 CInt -> [Window] -> Maybe Window
clickInsideBorder _ [] = Nothing
clickInsideBorder click (window:rest) = let borders = getBorderHitboxes window in
                                          if any (`pointInsideRectangle` click) borders
                                          then Just window
                                          else clickInsideBorder click rest

updateDrawingFun :: IO () -> Window -> GUI -> GUI
updateDrawingFun dFun w = updateGUI (modifyWindow $ w { drawFun = Just dFun})

-- Update a window in a gui multiple times
updateGUI' :: [Window -> GUI -> GUI] -> Window -> GUI -> GUI
updateGUI' funs w oldGUI = let wName = windowName w in
                                  foldl (\currentGUI f -> let newWindow = fromJust $ findWindowByName wName currentGUI in
                                                            f newWindow currentGUI) oldGUI funs

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

-- unfocus all windows on GUI
unfocusGUI :: GUI -> GUI
unfocusGUI = map (\window -> modifyWindow $ window { focused = False } )

setExpansion :: SDL.Point SDL.V2 CInt -> Window -> GUI -> GUI
setExpansion click oldWindow =
  let hitboxBorders = getBorderHitboxes oldWindow
      clickedHitbox = fromJust $ getFirst (`pointInsideRectangle` click) hitboxBorders
      -- Get the rectangle shown in the screen from the hitbox
      (SDL.Rectangle (SDL.P (SDL.V2 x' y')) _) = clickedHitbox
      borders = hitboxBorders
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

{- The windowName of the newWindow has to be the same as the old one.
  The safety window check should ensure that -}
updateGUI :: Window -> GUI -> GUI
updateGUI newWindow = go where
  go [] = []
  go (window:rest) = if windowName window == windowName newWindow
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
                                                          case clickInsideGUI clickCoords oldGUI of
                                                            Just w ->
                                                              sortGUI $
                                                              updateGUI' [\_ gui -> unfocusGUI gui,
                                                                           setDragging,
                                                                           (\wind gui ->
                                                                               updateGUI (modifyWindow $ wind { focused = True }) gui)] w oldGUI

                                                            Nothing -> case clickInsideBorder clickCoords oldGUI of
                                                                         Nothing -> oldGUI
                                                                         Just w -> setExpansion clickCoords w oldGUI
                                                | motion == SDL.Released
                                                  && button == SDL.ButtonLeft
                                                  && windowBeingDragged oldGUI =
                                                          let draggedWindow = fromJust $ getDraggedWindow oldGUI in
                                                            unsetDragging draggedWindow oldGUI
                                                | motion == SDL.Released
                                                  && button == SDL.ButtonLeft
                                                  && windowBeingExpanded oldGUI =
                                                          let expandedWindow = fromJust $ getExpandedWindow oldGUI in
                                                            unsetExpansion expandedWindow oldGUI
                                                | otherwise = oldGUI


guiHandleMotion :: SDL.V2 CInt -> GUI -> GUI
guiHandleMotion motion oldGUI | windowBeingDragged oldGUI = updateDraggedWindow motion (fromJust $ getDraggedWindow oldGUI) oldGUI
                              | windowBeingExpanded oldGUI = updateExpandedWindow motion (fromJust $ getExpandedWindow oldGUI) oldGUI
                              | otherwise = oldGUI

getMaxOffset :: Window -> CInt
getMaxOffset w = let (HTMLWindow (html, _, _)) = windowType w
                     renderTree = genRenderTree (dimensions w) html
                     (SDL.Rectangle (SDL.P (SDL.V2 _ y)) _) = dimensions w
                     (SDL.Rectangle (SDL.P (SDL.V2 _ y')) _) = last renderTree
                     maxOffset = y' - y - relativeMaxOffset in
                   maxOffset


guiHandleScrolling :: SDL.V2 CInt -> GUI -> IO GUI
guiHandleScrolling (SDL.V2 _ dy) oldGUI = do
  let dy' = -dy -- The default scrolling direction is a little weird so we need to flip it

  mouseCoords <- SDL.getAbsoluteMouseLocation
  case clickInsideGUI mouseCoords oldGUI of
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

loadVarTable :: [HTMLVar] -> String -> String
loadVarTable htmlVars html = foldl (flip loadVariable) html htmlVars

{- Meant to be called when you want to change the html that your window is based on -}
-- TODO: This will probably have some errors since the variables might disappear in the new rawText
reloadRawHTML :: SDL.Renderer -> SDL.Font.Font -> String -> Window -> GUI -> IO GUI
reloadRawHTML render font rawText w oldGUI =
  do let (HTMLWindow (_, _, (_, varTable))) = windowType w
         htmlText = loadVarTable varTable rawText
         newParsedHTML = fst $ head $ runParser parseHTML htmlText
     newTextures <- genHTMLTextures render font newParsedHTML
     return $ updateGUI
       (modifyWindow $ w { windowType = HTMLWindow (newParsedHTML, newTextures, (rawText, varTable))}) oldGUI

reloadHTMLVar :: SDL.Renderer -> SDL.Font.Font -> Window -> HTMLVar -> GUI -> IO GUI
reloadHTMLVar render font w (varName, varValue) oldGUI =
  do
  let (HTMLWindow (_, _, (rawText, varTable))) = windowType w
      newVarTable = fromMaybe (varTable ++ [(varName, varValue)]) $ lookupReplace varTable varName varValue
      htmlText' = loadVarTable newVarTable rawText
      newParsedHTML = fst $ head $ runParser parseHTML htmlText'
  newTextures <- genHTMLTextures render font newParsedHTML
  return $ updateGUI
    (modifyWindow $ w { windowType = HTMLWindow (newParsedHTML, newTextures, (rawText, newVarTable))}) oldGUI

{- Convenience function so we don't forget to load our variables
   Remember that the order that the variables are given DO matter! -}
loadHTMLFile :: FilePath -> [HTMLVar] -> IO String
loadHTMLFile fileName vars = do
  htmlText <- readFile fileName
  return $ go vars htmlText where
  go [] res = res
  go (htmlVar:rest) res = go rest (loadVariable htmlVar res)
