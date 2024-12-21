module Chelleport.AppShell (setupAppShell, MonadAppShell (..)) where

import Chelleport.Control (MonadControl (mouseButtonUp))
import Chelleport.Draw (colorBackground)
import Chelleport.Types
import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.RWS (MonadReader (ask))
import qualified Graphics.X11 as X11
import SDL (($=))
import qualified SDL
import System.Exit (exitSuccess)

class (Monad m) => MonadAppShell m where
  hideWindow :: m ()
  showWindow :: m ()
  shutdownApp :: m ()

instance (MonadIO m) => MonadAppShell (AppM m) where
  hideWindow = ask >>= SDL.hideWindow . ctxWindow
  showWindow = ask >>= SDL.showWindow . ctxWindow
  shutdownApp = do
    ctx <- ask
    SDL.destroyRenderer $ ctxRenderer ctx
    SDL.destroyWindow $ ctxWindow ctx
    mouseButtonUp
    liftIO $ do
      X11.closeDisplay $ ctxX11Display ctx
      exitSuccess

type Update state appAction = state -> appAction -> IO (state, Maybe appAction)

type EventHandler state appAction = state -> SDL.Event -> Maybe appAction

type View state = state -> IO ()

type Initializer state = IO state

setupAppShell ::
  DrawContext ->
  Initializer state ->
  Update state appAction ->
  EventHandler state appAction ->
  View state ->
  IO ()
setupAppShell ctx initState update eventHandler draw = do
  state <- initState
  appLoop state
  where
    appLoop state = do
      SDL.rendererDrawColor (ctxRenderer ctx) $= colorBackground
      SDL.clear $ ctxRenderer ctx
      draw state
      SDL.present $ ctxRenderer ctx

      events <- SDL.pollEvents
      newState <- foldM evaluateEvent state events

      appLoop newState

    evaluateEvent state event =
      maybe (pure state) (updateState state) (eventHandler state event)

    updateState state action =
      update state action >>= evalUpdateResult

    evalUpdateResult (state, Nothing) = pure state
    evalUpdateResult (state, Just action) = updateState state action
