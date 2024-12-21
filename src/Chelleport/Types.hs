module Chelleport.Types where

import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)
import Data.Word (Word8)
import qualified Graphics.X11 as X11
import qualified SDL
import qualified SDL.Font as TTF

type Cell = [Char]

type Color = SDL.V4 Word8

type KeySequence = [Char]

type KeyGrid = [[Cell]]

data State = State
  { stateGrid :: KeyGrid,
    stateKeySequence :: KeySequence,
    stateIsMatched :: Bool,
    stateIsShiftPressed :: Bool,
    stateIsDragging :: Bool,
    stateRepetition :: Int
  }
  deriving (Show, Eq)

data AppAction
  = ChainMouseClick MouseButtonType
  | HandleKeyInput SDL.Keycode
  | IncrementMouseCursor (Int, Int)
  | MouseDragStart
  | MouseDragEnd
  | MouseDragToggle
  | MoveMousePosition (Int, Int)
  | ResetKeys
  | ShutdownApp
  | TriggerMouseClick MouseButtonType
  | UpdateShiftState Bool
  | UpdateRepetition Int
  deriving (Show, Eq)

data DrawContext = DrawContext
  { ctxWindow :: SDL.Window,
    ctxRenderer :: SDL.Renderer,
    ctxFont :: TTF.Font,
    ctxX11Display :: X11.Display
  }

data MouseButtonType = LeftClick | RightClick
  deriving (Show, Eq)

newtype AppM m a = AppM {runAppM :: ReaderT DrawContext m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader DrawContext)
