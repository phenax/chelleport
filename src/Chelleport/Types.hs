module Chelleport.Types where

import qualified Graphics.X11 as X11
import qualified SDL
import qualified SDL.Font as TTF

type Cell = [Char]

type KeySequence = [Char]

type KeyGrid = [[Cell]]

data State = State
  { stateGrid :: KeyGrid,
    stateKeySequence :: KeySequence
  }

data AppAction
  = FilterSequence SDL.Keycode
  | MoveMousePosition (Int, Int)
  | ResetKeys
  | TriggerLeftClick

data DrawContext = DrawContext
  { ctxWindow :: SDL.Window,
    ctxRenderer :: SDL.Renderer,
    ctxFont :: TTF.Font,
    ctxX11Display :: X11.Display
  }
