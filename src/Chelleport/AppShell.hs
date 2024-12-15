module Chelleport.AppShell where

import Chelleport.Context (initializeContext)
import Chelleport.Draw (colorBackground)
import Chelleport.Types
import Control.Monad (foldM, unless)
import qualified Graphics.X11 as X11
import SDL (($=))
import qualified SDL
import qualified SDL.Font as TTF
import System.Exit (exitSuccess)

data Action act = SysQuit | AppAction act

newtype SysState = SysState {sysExit :: Bool}

type Update state appAction = state -> DrawContext -> appAction -> IO (state, Maybe (Action appAction))

type EventHandler state appAction = state -> SDL.Event -> Maybe (Action appAction)

type View state = state -> DrawContext -> IO ()

type Initializer state = DrawContext -> IO state

setupAppShell ::
  -- forall state appAction.
  Initializer state ->
  Update state appAction ->
  EventHandler state appAction ->
  View state ->
  IO ()
setupAppShell initState update eventHandler draw = do
  -- Initialize SDL
  SDL.initializeAll
  TTF.initialize
  ctx <- initializeContext
  state <- initState ctx

  appLoop ctx (state, SysState {sysExit = False})

  shutdownApp ctx
  where
    appLoop drawCtx (state, sysState) = do
      SDL.rendererDrawColor (ctxRenderer drawCtx) $= colorBackground
      SDL.clear $ ctxRenderer drawCtx
      draw state drawCtx
      SDL.present $ ctxRenderer drawCtx

      events <- SDL.pollEvents

      (newState, newSysState) <- foldM (evaluateEvent drawCtx) (state, sysState) events

      unless (sysExit newSysState) $
        appLoop drawCtx (newState, newSysState)

    evaluateEvent drawCtx stTup event =
      maybe (pure stTup) (updateState drawCtx stTup) (eventHandler (fst stTup) event)

    evalUpdateResult _drawCtx sysState (state, Nothing) = pure (state, sysState)
    evalUpdateResult drawCtx sysState (state, Just action) = updateState drawCtx (state, sysState) action

    updateState _ (state, sysState) SysQuit = pure (state, sysState {sysExit = True})
    updateState drawCtx (state, sysState) (AppAction action) =
      update state drawCtx action >>= evalUpdateResult drawCtx sysState

hideWindow :: DrawContext -> IO ()
hideWindow ctx = SDL.hideWindow (ctxWindow ctx)

closeWindow :: DrawContext -> IO ()
closeWindow ctx = do
  SDL.destroyRenderer $ ctxRenderer ctx
  SDL.destroyWindow $ ctxWindow ctx

shutdownApp :: DrawContext -> IO ()
shutdownApp ctx = do
  closeWindow ctx
  X11.closeDisplay $ ctxX11Display ctx
  exitSuccess
