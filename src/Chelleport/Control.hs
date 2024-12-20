module Chelleport.Control where

import Chelleport.Types
import Chelleport.Utils (cIntToInt)
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask))
import Foreign.C (CInt)
import qualified Graphics.X11 as X11
import qualified Graphics.X11.XTest as X11
import qualified SDL

class (Monad m) => MonadControl m where
  pressMouseButton :: MouseButtonType -> m ()
  moveMousePointer :: CInt -> CInt -> m ()
  getMousePointerPosition :: m (CInt, CInt)

instance (MonadIO m) => MonadControl (AppM m) where
  pressMouseButton btn = do
    (DrawContext {ctxX11Display = display}) <- ask
    liftIO $ do
      -- Wrap with delay to prevent async window close issues. TODO: Remove maybe?
      threadDelay 30_000
      X11.fakeButtonPress display x11Button
      X11.sync display False
      threadDelay 30_000
    where
      x11Button = case btn of
        LeftClick -> X11.button1

  moveMousePointer x y = do
    DrawContext {ctxWindow = window} <- ask
    SDL.warpMouse (SDL.WarpInWindow window) (SDL.P $ SDL.V2 x y)

  getMousePointerPosition = do
    (SDL.P (SDL.V2 x y)) <- SDL.getAbsoluteMouseLocation
    pure (x, y)

isKeyPress :: SDL.KeyboardEventData -> Bool
isKeyPress = (== SDL.Pressed) . SDL.keyboardEventKeyMotion

isKeyRelease :: SDL.KeyboardEventData -> Bool
isKeyRelease = (== SDL.Released) . SDL.keyboardEventKeyMotion

eventToKeycode :: SDL.KeyboardEventData -> SDL.Keycode
eventToKeycode = SDL.keysymKeycode . SDL.keyboardEventKeysym

isKeyPressWith :: SDL.KeyboardEventData -> SDL.Keycode -> Bool
isKeyPressWith keyboardEvent keyCode =
  isKeyPress keyboardEvent && eventToKeycode keyboardEvent == keyCode

isKeyReleaseWith :: SDL.KeyboardEventData -> SDL.Keycode -> Bool
isKeyReleaseWith keyboardEvent keyCode =
  isKeyRelease keyboardEvent && eventToKeycode keyboardEvent == keyCode

directionalIncrement :: (CInt, CInt) -> Char -> (Int, Int)
directionalIncrement (incX, incY) = \case
  'H' -> (-cIntToInt incX, 0)
  'L' -> (cIntToInt incX, 0)
  'K' -> (0, -cIntToInt incY)
  'J' -> (0, cIntToInt incY)
  _ -> undefined
