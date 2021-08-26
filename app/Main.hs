{-# LANGUAGE OverloadedStrings #-}

import Draw
import DataTypes
import Parser
import WindowUtils
import Constants

import qualified SDL
import qualified SDL.Font

import SDL.Video.Renderer
import Control.Monad

{- TODO:
  Add component safety checks
  Split main file
  Add dynamic events to the windows
  break tag parameters
  weird rectangles
-}

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

  {-
  if any handleMouse events then
    print $ any handleMouse events
  else return ()
  -}

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

  unparsedHTML <- readFile "../assets/testFile.html"
  let parsedHTML = fst $ head $ runParser parseHTML unparsedHTML
  htmlTexts <- genHTMLTextures renderer font parsedHTML

  SDL.Font.setHinting font SDL.Font.Mono
  size <- SDL.Font.getHinting font

  let startGUI = createGUIWindow (HTMLWindow (parsedHTML, htmlTexts)) (Rectangle (SDL.P (SDL.V2 0 0)) (SDL.V2 500 500))
                        10 white blue [] emptyGUI

  loop renderer font startGUI
  SDL.destroyWindow window
