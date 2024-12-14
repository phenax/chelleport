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
  -- bounds <- fmap SDL.displayBoundsSize <$> SDL.getDisplays
  -- let windowSize = case bounds of
  --       (x : _) -> x
  --       _ -> SDL.V2 800 600
  let windowSize = SDL.V2 0 0

  let windowCfg =
        SDL.defaultWindow
          { SDL.windowInputGrabbed = True,
            SDL.windowMode = SDL.FullscreenDesktop,
            SDL.windowPosition = SDL.Absolute $ SDL.P $ SDL.V2 0 0,
            SDL.windowInitialSize = windowSize,
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

      SDL.clear $ ctxRenderer drawCtx
      draw state drawCtx
      SDL.present $ ctxRenderer drawCtx

      (newState, newSysState) <- foldM (evaluateEvent drawCtx) (state, sysState) events
      unless (sysExit newSysState) $ appLoop drawCtx (newState, newSysState)

    evaluateEvent drawCtx stTup event = maybe (pure stTup) (updateState drawCtx stTup) (eventHandler (fst stTup) event)

    updateState _drawCtx (state, sysState) SysQuit = pure (state, sysState {sysExit = True})
    updateState drawCtx (state, sysState) (AppAction action) = (,sysState) <$> update state drawCtx action
