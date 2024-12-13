module Chelleport where

import Control.Monad (foldM, unless)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Foreign (Ptr)
import Foreign.C (CInt)
import SDL (($=))
import qualified SDL
import qualified SDL.Font as TTF
import System.Exit (exitSuccess)

open :: IO ()
open = do
  -- Initialize SDL
  SDL.initializeAll
  TTF.initialize

  -- Create window
  window <-
    SDL.createWindow
      "My SDL Application"
      ( SDL.defaultWindow
          { -- SDL.windowMode = SDL.Fullscreen,
            SDL.windowInputGrabbed = True,
            SDL.windowBorder = False
          }
      )

  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  font <- TTF.load "Inter-Regular.ttf" 16

  SDL.windowOpacity window $= 0.6
  SDL.rendererDrawBlendMode renderer $= SDL.BlendAlphaBlend
  SDL.rendererDrawColor renderer $= SDL.V4 0 0 0 0

  appLoop renderer initState font

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  pure ()

data State = State
  { stateExit :: Bool,
    stateCount :: Int
  }

data Action = ActionQuit | ActionUpdateCount Int

initState :: State
initState = State {stateExit = False, stateCount = 0}

appLoop :: SDL.Renderer -> State -> TTF.Font -> IO ()
appLoop renderer state font = do
  events <- SDL.pollEvents

  SDL.clear renderer
  renderText renderer font $ Text.pack ("Hello, Haskell: " ++ show (stateCount state))
  SDL.present renderer

  newState <- evaluateEvents state events
  unless (stateExit newState) (appLoop renderer newState font)

evaluateEvents :: State -> [SDL.Event] -> IO State
evaluateEvents = foldM evaluate
  where
    evaluate :: State -> SDL.Event -> IO State
    evaluate st event = maybe (pure st) (update st) (eventToAction event)

update :: State -> Action -> IO State
update state ActionQuit = pure state {stateExit = True}
update state (ActionUpdateCount count) = pure state {stateCount = stateCount state + count}

eventToAction :: SDL.Event -> Maybe Action
eventToAction event =
  case SDL.eventPayload event of
    SDL.KeyboardEvent keyboardEvent
      | isKeyPress keyboardEvent SDL.KeycodeQ -> Just ActionQuit
    SDL.KeyboardEvent keyboardEvent
      | isKeyPress keyboardEvent SDL.KeycodeJ -> Just $ ActionUpdateCount (-1)
    SDL.KeyboardEvent keyboardEvent
      | isKeyPress keyboardEvent SDL.KeycodeK -> Just $ ActionUpdateCount 1
    _ -> Nothing

isKeyPress :: SDL.KeyboardEventData -> SDL.Keycode -> Bool
isKeyPress keyboardEvent keyCode =
  SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed
    && SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == keyCode

-- Render text to the screen
renderText :: SDL.Renderer -> TTF.Font -> Text -> IO ()
renderText renderer font text = do
  -- Render text in white
  surface <- TTF.blended font (SDL.V4 255 255 255 255) text
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface

  -- Get text dimensions
  textureInfo <- SDL.queryTexture texture
  let textWidth = SDL.textureWidth textureInfo
      textHeight = SDL.textureHeight textureInfo
      position =
        SDL.P (SDL.V2 ((640 - textWidth) `div` 2) ((480 - textHeight) `div` 2))

  -- Render the texture
  SDL.copy renderer texture Nothing (Just (SDL.Rectangle position (SDL.V2 textWidth textHeight)))
  SDL.destroyTexture texture
