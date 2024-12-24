module Chelleport.AppShell (setupAppShell, MonadAppShell (..)) where

import Chelleport.Config
import Chelleport.Control (MonadControl (releaseMouseButton))
import Chelleport.Types
import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.RWS (MonadReader (ask), asks)
import qualified Graphics.X11 as X11
import SDL (($=))
import qualified SDL
import System.Exit (exitSuccess)

class (Monad m) => MonadAppShell m where
  hideWindow :: m ()
  showWindow :: m ()
  shutdownApp :: m ()

instance (MonadIO m) => MonadAppShell (AppM m) where
  hideWindow = asks ctxWindow >>= SDL.hideWindow
  showWindow = asks ctxWindow >>= SDL.showWindow
  shutdownApp = do
    DrawContext {ctxRenderer = renderer, ctxWindow = window, ctxX11Display = x11Display} <- ask
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    releaseMouseButton
    SDL.quit
    liftIO $ do
      X11.closeDisplay x11Display
      exitSuccess

type Update m state appAction = state -> appAction -> m (state, Maybe appAction)

type EventHandler state appAction = state -> SDL.Event -> Maybe appAction

type View m state = state -> m ()

type Initializer m state appAction = m (state, Maybe appAction)

setupAppShell ::
  (MonadIO m) =>
  DrawContext ->
  Initializer m state appAction ->
  Update m state appAction ->
  EventHandler state appAction ->
  View m state ->
  m ()
setupAppShell (DrawContext {ctxRenderer = renderer}) getInitState update eventHandler draw = do
  state <- getInitState >>= evalUpdateResult
  appLoop state
  where
    appLoop currentState = do
      SDL.rendererDrawColor renderer $= colorBackground
      SDL.clear renderer
      draw currentState
      SDL.present renderer

      newState <- SDL.pollEvents >>= foldM evaluateEvent currentState

      appLoop newState

    evaluateEvent state event =
      maybe (pure state) (updateState state) (eventHandler state event)

    updateState state action =
      update state action >>= evalUpdateResult

    evalUpdateResult (state, Nothing) = pure state
    evalUpdateResult (state, Just action) = updateState state action
