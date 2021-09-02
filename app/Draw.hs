module Draw where

import Constants
import DataTypes
import Parser
import WindowUtils

import qualified SDL
import SDL.Font
import Control.Monad
import Foreign.C.Types

import qualified Data.Text as T

genRenderTree :: SDL.Rectangle CInt -> [HTML] -> [SDL.Rectangle CInt]
genRenderTree drawRect htmlDOC = snd $ foldl (\(oldDrawRect, prevRenderNodes) html ->
                                                let (newDrawRect, newRenderNode) = genRenderNode oldDrawRect html in
                                                  (newDrawRect, mappend prevRenderNodes [newRenderNode])) (drawRect, []) htmlDOC

genRenderNode :: SDL.Rectangle CInt -> HTML -> (SDL.Rectangle CInt, SDL.Rectangle CInt)
genRenderNode (SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 dx dy)) (Paragraph p _ fontSize) =
    let lineHSize = div dx (fromIntegral fontSize)
        lineVSize = fontSize
        amountOfLines = div (length p) (fromIntegral lineHSize)
        paragraphSize = fromIntegral $ amountOfLines * fromIntegral lineVSize
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


--TODO: Add safety checks to drawings outside the draw rectangle
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

-- Draws a HTML Tag, "consuming" a texture if needed
drawHTML :: SDL.Renderer -> Window -> SDL.Rectangle CInt -> [[SDL.Texture]] -> HTML -> IO [[SDL.Texture]]
drawHTML render w drawRect (textures:rest) (Paragraph p color fontSize) = do

  infoTexts <- mapM (\text -> do
                        textInfo <- SDL.queryTexture text
                        return (text, textInfo)) textures
  let drawFrame = dimensions w
      sdRects = filter (rectInsideRect drawFrame . snd) $ srcDestRects fontSize (T.length $ T.pack p) infoTexts drawRect
      dRects = map snd sdRects
  SDL.rendererDrawColor render SDL.$= Constants.red

  mapM_ (\((text, s), d) -> do
            SDL.copy render text (Just s) (Just d)) sdRects

  mapM_ (\d -> do
            SDL.rendererDrawColor render SDL.$= Constants.red
            SDL.drawRect render (Just d)) dRects

  return rest

drawHTML _ _ _ textures _ = return textures

--drawHTML render drawRect (textures) (Break) = return textures

drawWindowContents :: SDL.Renderer -> Window -> SDL.Font.Font -> SDL.Rectangle CInt -> WType -> IO ()
drawWindowContents render w _ drawRect (HTMLWindow (htmlDOC, texts)) = do
  let renderTree = genRenderTree drawRect htmlDOC
  print renderTree
  foldM_ (\textures (html, rect) -> do
                 drawHTML render w rect textures html) texts (zip htmlDOC renderTree)

drawWindow :: SDL.Renderer -> SDL.Font.Font -> Window -> IO ()
drawWindow render font w = do
  let drawRect = dimensions w
      bColor = borderColor w
      (SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 dx _)) = drawRect
      wType = windowType w
      bSize = borderSize w
      frameBorders = getRectBorders bSize drawRect

  SDL.rendererDrawColor render SDL.$= bColor

  mapM_ (SDL.fillRect render . Just) frameBorders --Draw Main Frame

  drawWindowContents render w font drawRect wType

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

drawGUI :: SDL.Renderer -> SDL.Font.Font -> GUI -> IO ()
drawGUI render font gui = mapM_ (drawWindow render font) (windows gui)
