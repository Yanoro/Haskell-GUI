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
      window = head $ windows newGUI
      handleMouse event =
        case SDL.eventPayload event of
          SDL.MouseButtonEvent (SDL.MouseButtonEventData _ motion _ button _ coords) ->
            do
            mapM_ (print) $ getRectBorders 10 $ dimensions window
            print coords
--            print $ getFirst (`insideRectangle` (fromIntegral <$> coords)) $ getRectBorders 10 $ dimensions window
          _ -> return ()

--          _ -> False
      quitKeyPressed = any quitKey events
      newGUI = foldr (guiHandleEvent . SDL.eventPayload) gui events

  {-
  if any handleMouse events then
    print $ any handleMouse events
  else return ()
  -}
  mapM_ handleMouse events
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

  unparsedHTML <- loadHTMLFile "../assets/testFile.html" [("$VAR1", "Death"), ("$VAR2", "10"), ("$VAR3", "LIFE")]

  let parsedHTML = fst $ head $ runParser parseHTML unparsedHTML
  htmlTexts <- genHTMLTextures renderer font parsedHTML

  SDL.Font.setHinting font SDL.Font.Mono

  let startGUI = createGUIWindow (HTMLWindow (parsedHTML, htmlTexts)) (Rectangle (SDL.P (SDL.V2 0 0)) (SDL.V2 500 500))
                 10 10 white blue [] emptyGUI

  loop renderer font startGUI
  SDL.destroyWindow window
