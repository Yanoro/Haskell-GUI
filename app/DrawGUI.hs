module DrawGUI where

import GUIConstants
import GUIDataTypes
import GUIParser
import WindowUtils

import qualified SDL
import SDL.Font
import Control.Monad
import Foreign.C.Types

import qualified Data.Text as T

{- Gets adequate source and destination rectangles to properly display text -}
srcDestRects :: FontSize -> Int -> [(SDL.Texture, SDL.TextureInfo)] -> SDL.Rectangle CInt -> [((SDL.Texture, SDL.Rectangle CInt), SDL.Rectangle CInt)]
srcDestRects fSize msgLength texts rect = let dRecs = destRects fSize msgLength rect
                                              sRecs = srcRects fSize msgLength texts dRecs in
                                      zip sRecs dRecs

srcRects :: FontSize -> Int -> [(SDL.Texture, SDL.TextureInfo)] -> [SDL.Rectangle CInt] -> [(SDL.Texture, SDL.Rectangle CInt)]
srcRects fSize msgLen = go 0 where
  go x' texts dRects  | null dRects = [(text, SDL.Rectangle (SDL.P (SDL.V2 x' 0)) (SDL.V2 (x' - x) y))]
                      | x' >= x = go 0 (tail texts) dRects
                      | otherwise = (text, SDL.Rectangle (SDL.P (SDL.V2 x' 0)) (SDL.V2 textLineSize y)) : go nextX
                                                                                                        texts
                                                                                                        (tail dRects)
    where nextX = x' + textLineSize
          (SDL.Rectangle _ (SDL.V2 dx _)) = head dRects
          fontSize = fromIntegral fSize
          lineSize = div dx fontSize
          hdTexts = head texts
          (text, textInfo) = hdTexts
          (x, y) = (SDL.textureWidth textInfo, SDL.textureHeight textInfo)
          charSize = div x (fromIntegral msgLen)
          textLineSize = charSize * lineSize

destRects :: FontSize -> Int -> SDL.Rectangle CInt -> [SDL.Rectangle CInt]
destRects fSize msgLen (SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 dx _)) =
  go 0 msgLen where
  lineSize = div dx (fromIntegral fSize)
  go :: CInt -> Int -> [SDL.Rectangle CInt]
  go n msgRem | msgRem == 0 = []
              | msgRem <= fromIntegral lineSize = [SDL.Rectangle (SDL.P (SDL.V2 x (y + fontSize * n)))
                                                   (SDL.V2 (fromIntegral $ fSize * (fromIntegral msgRem + 1)) fontSize)]
              | otherwise = SDL.Rectangle (SDL.P (SDL.V2 x (y + fontSize * n))) (SDL.V2 dx fontSize)  : go (n + 1)
                                                                                              (msgRem - fromIntegral lineSize)
                            where fontSize = fromIntegral fSize

drawText :: SDL.Renderer -> Window -> SDL.Rectangle CInt -> [SDL.Texture] -> String -> CInt -> IO ()
drawText render w drawRect textures msg fontSize = do
  infoTexts <- mapM (\text -> do
                        textInfo <- SDL.queryTexture text
                        return (text, textInfo)) textures
  let drawFrame = dimensions w
      offset = scrollingOffset w
      sdRects = filter (rectInsideRect drawFrame . snd) $
                map (\(a, SDL.Rectangle (SDL.P (SDL.V2 x y)) d) -> (a, SDL.Rectangle (SDL.P (SDL.V2 x (y - offset))) d))
                $ srcDestRects fontSize (T.length $ T.pack msg) infoTexts drawRect
      dRects = map snd sdRects

  mapM_ (\((text, s), d) -> do
            SDL.copy render text (Just s) (Just d)) sdRects

  mapM_ (\d -> do
            SDL.rendererDrawColor render SDL.$= GUIConstants.red
            SDL.drawRect render (Just d)) dRects


-- Interprets the point as coordinates relative to the top left corner of the draw rect
turnPointRelative :: SDL.Point SDL.V2 CInt -> Window -> Maybe (SDL.Point SDL.V2 CInt)
turnPointRelative (SDL.P (SDL.V2 x y)) w = let (SDL.Rectangle (SDL.P (SDL.V2 n m)) (SDL.V2 dn dm)) = dimensions w in
                                             if x + n > n + dn || y + m > m + dm
                                             then Nothing else Just $ SDL.P (SDL.V2 (x + n) (y + m))

drawInsideOfWindow :: (SDL.Rectangle CInt -> IO ()) -> SDL.Rectangle CInt -> Window -> IO ()
drawInsideOfWindow drawFun (SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 dx dy)) w =
  do let (SDL.Rectangle (SDL.P (SDL.V2 n m)) (SDL.V2 nx mx)) = dimensions w
         adjustedRect = SDL.Rectangle (SDL.P (SDL.V2 (x + n) (y + m))) (SDL.V2 dx dy)
     drawFun adjustedRect

-- Draws a HTML Tag, "consuming" a texture if needed
drawHTML :: SDL.Renderer -> Window -> SDL.Rectangle CInt -> [[SDL.Texture]] -> HTML -> IO [[SDL.Texture]]
drawHTML render w drawRect (current_texture:rest) (Paragraph msg color fontSize) = do
  -- mapM_ (\(msg, texture) -> drawText render w drawRect texture msg fontSize) textures
  drawText render w drawRect current_texture msg fontSize
  return rest

drawHTML _ _ _ textures _ = return textures


-- TODO: Maybe remove the font argument and drawRect?
drawWindowContents :: SDL.Renderer -> Window -> SDL.Font.Font -> SDL.Rectangle CInt -> WType -> IO ()
drawWindowContents render w _ drawRect (HTMLWindow (htmlDOC, texts, _)) = do
  let renderTree = genRenderTree drawRect htmlDOC

  foldM_ (\textures (html, rect) -> do
                 drawHTML render w rect textures html) texts (zip htmlDOC renderTree)

drawWindowContents render w _ drawRect StandardWindow = return ()

drawWindow :: SDL.Renderer -> SDL.Font.Font -> Window -> IO ()
drawWindow render font w = do
  let drawRect = dimensions w
      bColor = borderColor w
      backColor = backgroundColor w
      drawingFun = drawFun w

      (SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 dx _)) = drawRect
      wType = windowType w
      bSize = borderSize w
      frameBorders = getRectBorders bSize drawRect

  SDL.rendererDrawColor render SDL.$= bColor

  mapM_ (SDL.fillRect render . Just) frameBorders --Draw Main Frame

--  SDL.rendererDrawColor render SDL.$= red
--  mapM_ (SDL.fillRect render . Just) hitboxes

  SDL.rendererDrawColor render SDL.$= backColor

  SDL.fillRect render (Just drawRect)

  drawWindowContents render w font drawRect wType

  case drawingFun of
    Nothing -> return ()
    Just fun -> do fun
                   return ()

{- This function should usually be the last drawing function that the gui windows are not transparent-}
drawGUI :: SDL.Renderer -> SDL.Font.Font -> GUI -> IO ()
drawGUI render font = mapM_ (drawWindow render font)
