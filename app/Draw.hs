module Draw where

import DataTypes

import qualified SDL
import SDL.Font
import Foreign.C.Types

import qualified Data.Text as T

red :: DataTypes.Color
red = SDL.V4 255 0 0 0

fontSize :: CInt
fontSize = 12

{- Gets adequate source and destination rectangles to properly display text -}
srcDestRects :: Int -> [(SDL.Texture, SDL.TextureInfo)] -> SDL.Rectangle CInt -> [((SDL.Texture, SDL.Rectangle CInt), SDL.Rectangle CInt)]
srcDestRects msgLength texts rect = let dRecs = destRects msgLength rect
                                        sRecs = srcRects msgLength texts dRecs in
                                      zip sRecs dRecs

srcRects :: Int -> [(SDL.Texture, SDL.TextureInfo)] -> [SDL.Rectangle CInt] -> [(SDL.Texture, SDL.Rectangle CInt)]
srcRects msgLen = go 0 where
  go x' texts destRects  | null destRects = [(text, SDL.Rectangle (SDL.P (SDL.V2 x' 0)) (SDL.V2 (x' - x) y))]
                         | x' >= x = go 0 (tail texts) destRects
                         | otherwise = (text, SDL.Rectangle (SDL.P (SDL.V2 x' 0)) (SDL.V2 textLineSize y)) : go nextX
                                                                                                        texts
                                                                                                        (tail destRects)
    where nextX = x' + textLineSize
          (SDL.Rectangle _ (SDL.V2 dx _)) = head destRects
          lineSize = div dx fontSize
          hdTexts = head texts
          (text, textInfo) = hdTexts
          (x, y) = (SDL.textureWidth textInfo, SDL.textureHeight textInfo)
          charSize = div x (fromIntegral msgLen)
          textLineSize = charSize * lineSize

destRects :: Int -> SDL.Rectangle CInt -> [SDL.Rectangle CInt]
destRects msgLen (SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 dx dy)) =
  go 0 msgLen where
  lineSize = div dx fontSize
  go :: CInt -> Int -> [SDL.Rectangle CInt]
  go n msgRem | msgRem == 0 = []
              | msgRem <= fromIntegral lineSize = [SDL.Rectangle (SDL.P (SDL.V2 x (y + 12 * n)))
                                                   (SDL.V2 (fromIntegral $ 12 * (msgRem + 1)) fontSize)]
              | otherwise = SDL.Rectangle (SDL.P (SDL.V2 x (y + 12 * n))) (SDL.V2 dx fontSize)  : go (n + 1)
                                                                                              (msgRem - fromIntegral lineSize)

getDrawingRect :: Window -> SDL.Rectangle CInt
getDrawingRect w = let (SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 dx dy)) = dimensions w
                       tBarH = titleBarHeight w in
                     SDL.Rectangle (SDL.P (SDL.V2 (x + 1) (y + tBarH))) (SDL.V2 (dx - 2) (dy - tBarH - 1))

drawComponent :: SDL.Renderer -> SDL.Font.Font -> (Component, SDL.Rectangle CInt, Window -> Window) -> IO ()
drawComponent render _ (Button color, rect, _) = do
  SDL.rendererDrawColor render SDL.$= color
  SDL.drawRect render (Just rect)

drawComponent render _ (Text msg color texts, rect, _) = do
  SDL.rendererDrawColor render SDL.$= color
  infoTexts <- mapM (\text -> do
                        textInfo <- SDL.queryTexture text
                        return (text, textInfo)) texts

  let sdRects = srcDestRects (T.length msg) infoTexts rect
      dRects = map snd sdRects

  mapM_ (\((text, s), d) -> do
            SDL.copy render text (Just s) (Just d)) sdRects

  mapM_ (\d -> do
            SDL.rendererDrawColor render SDL.$= red
            SDL.drawRect render (Just d)) dRects

  SDL.drawRect render (Just rect)

drawWindow :: SDL.Renderer -> SDL.Font.Font -> Window -> IO ()
drawWindow render font w = do
  let outerRect = dimensions w
      bColor = borderColor w
      tColor = titleBarFillColor w
      (SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 dx _)) = outerRect
      relTBarH = titleBarHeight w
      absTBarH = relTBarH + y

      -- We need those additions and subtractions so lines don't go over where they should go
      (titleBarStart, titleBarEnd) = (SDL.P (SDL.V2 x absTBarH), SDL.P (SDL.V2 (x + dx - 1) absTBarH))
      titleBarColorRect = SDL.Rectangle (SDL.P (SDL.V2 (x + 1) (y + 1))) (SDL.V2 (dx - 2) (relTBarH - 1))

      comps = components w

  SDL.rendererDrawColor render SDL.$= bColor
  SDL.drawRect render (Just outerRect) --Draw Main Frame

  SDL.drawLine render titleBarStart titleBarEnd -- Draw Title Bar line

  SDL.rendererDrawColor render SDL.$= tColor
  SDL.fillRect render (Just titleBarColorRect) -- Fill tile bar with color

  mapM_ (drawComponent render font) comps

drawGUI :: SDL.Renderer -> SDL.Font.Font -> GUI -> IO ()
drawGUI render font gui = mapM_ (drawWindow render font) (windows gui)
