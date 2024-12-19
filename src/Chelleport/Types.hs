module Chelleport.Types where

import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)
import qualified Graphics.X11 as X11
import qualified SDL
import qualified SDL.Font as TTF

type Cell = [Char]

type KeySequence = [Char]

type KeyGrid = [[Cell]]

data State = State
  { stateGrid :: KeyGrid,
    stateKeySequence :: KeySequence,
    stateIsMatched :: Bool,
    stateIsShiftPressed :: Bool
  }

data AppAction
  = HandleKeyInput SDL.Keycode
  | MoveMousePosition (Int, Int)
  | ResetKeys
  | TriggerLeftClick
  | IncrementMouseCursor (Int, Int)
  | ShutdownApp
  | UpdateShiftState Bool

data DrawContext = DrawContext
  { ctxWindow :: SDL.Window,
    ctxRenderer :: SDL.Renderer,
    ctxFont :: TTF.Font,
    ctxX11Display :: X11.Display
  }

type Update state appAction = state -> appAction -> IO (state, Maybe appAction)

type EventHandler state appAction = state -> SDL.Event -> Maybe appAction

type View state = state -> IO ()

type Initializer state = IO state

data MouseButtonType = LeftClick

newtype AppM m a = AppM {runAppM :: ReaderT DrawContext m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader DrawContext)
