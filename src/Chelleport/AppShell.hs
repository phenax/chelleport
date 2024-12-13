module Chelleport.AppShell where

import Control.Monad (foldM, unless)
import SDL (($=))
import qualified SDL
import qualified SDL.Font as TTF

data Action act = SysQuit | AppAction act

newtype SysState = SysState {sysExit :: Bool}

data DrawContext = DrawContext
  { ctxWindow :: SDL.Window,
    ctxRenderer :: SDL.Renderer,
    ctxFont :: TTF.Font
  }

createContext :: IO DrawContext
createContext = do
  let windowCfg =
        SDL.defaultWindow -- SDL.windowMode = SDL.Fullscreen,
          { SDL.windowInputGrabbed = True,
            SDL.windowBorder = False
          }
  window <- SDL.createWindow "My SDL Application" windowCfg
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  font <- TTF.load "Inter-Regular.ttf" 16

  SDL.windowOpacity window $= 0.6
  SDL.rendererDrawBlendMode renderer $= SDL.BlendAlphaBlend
  SDL.rendererDrawColor renderer $= SDL.V4 0 0 0 0

  pure $ DrawContext {ctxWindow = window, ctxRenderer = renderer, ctxFont = font}

setupAppShell ::
  state ->
  (state -> DrawContext -> appAction -> IO state) ->
  (state -> SDL.Event -> Maybe (Action appAction)) ->
  (state -> DrawContext -> IO ()) ->
  IO ()
setupAppShell initState update eventHandler draw = do
  -- Initialize SDL
  SDL.initializeAll
  TTF.initialize

  ctx <- createContext
  appLoop ctx (initState, SysState {sysExit = False})

  SDL.destroyRenderer $ ctxRenderer ctx
  SDL.destroyWindow $ ctxWindow ctx
  where
    appLoop drawCtx (state, sysState) = do
      events <- SDL.pollEvents

      SDL.clear $ ctxRenderer drawCtx
      draw state drawCtx
      SDL.present $ ctxRenderer drawCtx

      (newState, newSysState) <- foldM (evaluateEvent drawCtx) (state, sysState) events
      unless (sysExit newSysState) $ appLoop drawCtx (newState, newSysState)

    evaluateEvent drawCtx stTup event = maybe (pure stTup) (updateState drawCtx stTup) (eventHandler (fst stTup) event)

    updateState _drawCtx (state, sysState) SysQuit = pure (state, sysState {sysExit = True})
    updateState drawCtx (state, sysState) (AppAction action) = (,sysState) <$> update state drawCtx action
