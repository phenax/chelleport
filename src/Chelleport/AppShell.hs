module Chelleport.AppShell where

import Chelleport.Context (DrawContext (ctxRenderer, ctxWindow), createContext)
import Control.Monad (foldM, unless)
import SDL (($=))
import qualified SDL
import qualified SDL.Font as TTF

data Action act = SysQuit | AppAction act

newtype SysState = SysState {sysExit :: Bool}

setupAppShell ::
  (DrawContext -> IO state) ->
  (state -> DrawContext -> appAction -> IO state) ->
  (state -> SDL.Event -> Maybe (Action appAction)) ->
  (state -> DrawContext -> IO ()) ->
  IO ()
setupAppShell initState update eventHandler draw = do
  -- Initialize SDL
  SDL.initializeAll
  TTF.initialize

  ctx <- createContext
  state <- initState ctx
  appLoop ctx (state, SysState {sysExit = False})

  SDL.destroyRenderer $ ctxRenderer ctx
  SDL.destroyWindow $ ctxWindow ctx
  where
    appLoop drawCtx (state, sysState) = do
      events <- SDL.pollEvents

      SDL.rendererDrawColor (ctxRenderer drawCtx) $= SDL.V4 0 0 0 0
      SDL.clear $ ctxRenderer drawCtx
      draw state drawCtx
      SDL.present $ ctxRenderer drawCtx

      (newState, newSysState) <- foldM (evaluateEvent drawCtx) (state, sysState) events
      unless (sysExit newSysState) $ appLoop drawCtx (newState, newSysState)

    evaluateEvent drawCtx stTup event = maybe (pure stTup) (updateState drawCtx stTup) (eventHandler (fst stTup) event)

    updateState _drawCtx (state, sysState) SysQuit = pure (state, sysState {sysExit = True})
    updateState drawCtx (state, sysState) (AppAction action) = (,sysState) <$> update state drawCtx action
