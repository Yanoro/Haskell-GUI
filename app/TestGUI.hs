{-# LANGUAGE OverloadedStrings #-}

module TestGUI where

import DrawGUI
import GUIDataTypes
import GUIParser
import WindowUtils


import qualified SDL
import qualified SDL.Font

import SDL.Video.Renderer
import Control.Monad

{- TODO
  Handle what happens with multiple windows with the same name
  Add dynamic events to the windows
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
          SDL.MouseButtonEvent (SDL.MouseButtonEventData _ motion _ button _ coords) ->
            True
          _ -> False

      quitKeyPressed = any quitKey events

  newGUI <- foldM (flip guiHandleEvent) gui $ map SDL.eventPayload events

  {-
  gui' <- if any handleMouse events then
          reloadHTMLVar render font window ("$VAR1", "Life") newGUI
          else return newGUI
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

  let vars = [("$VAR1", "Death"), ("$VAR2", "10"), ("$VAR3", "LIFE")]
      fileName = "../assets/testFile.html"
  rawHTML <- loadHTMLFile fileName vars

  let parsedHTML = fst $ head $ runParser parseHTML rawHTML
  htmlTexts <- genHTMLTextures renderer font parsedHTML

  SDL.Font.setHinting font SDL.Font.Mono

  let startGUI = createGUIWindow "Start Window" (HTMLWindow (parsedHTML, htmlTexts, (rawHTML, vars))) Nothing Nothing Nothing Nothing
                  Nothing Nothing Nothing Nothing emptyGUI

  loop renderer font startGUI
  SDL.destroyWindow window
