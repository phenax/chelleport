module Chelleport.AppShell where

import Chelleport.Config
import Chelleport.Control (MonadControl (releaseMouseButton))
import Chelleport.Types
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
    ctx <- ask
    SDL.destroyRenderer $ ctxRenderer ctx
    SDL.destroyWindow $ ctxWindow ctx
    releaseMouseButton
    SDL.quit
    liftIO $ do
      X11.closeDisplay $ ctxX11Display ctx
      exitSuccess

type Flush m = m ()

type Update m state appAction = Flush m -> state -> appAction -> m (state, Maybe appAction)

type EventHandler state appAction = state -> SDL.Event -> Maybe appAction

type View m state = state -> m ()

type Initializer m state appAction = m (state, Maybe appAction)

setupAppShell ::
  (MonadIO m, Show state) =>
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
      renderScreen currentState
      newState <- SDL.waitEvent >>= evaluateEvent currentState
      appLoop newState

    renderScreen state = do
      SDL.rendererDrawColor renderer $= colorBackground
      SDL.clear renderer
      draw state
      SDL.present renderer

    evaluateEvent state event =
      maybe (pure state) (updateState state) (eventHandler state event)

    updateState state action =
      update (renderScreen state) state action >>= evalUpdateResult

    evalUpdateResult (state, Nothing) = pure state
    evalUpdateResult (state, Just action) = updateState state action
