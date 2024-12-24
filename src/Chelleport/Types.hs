module Chelleport.Types where

import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)
import Data.Vector.Storable (Storable)
import Data.Word (Word8)
import Foreign (Ptr, Storable (alignment, peek, poke, sizeOf), castPtr, nullPtr, plusPtr)
import Foreign.C (CChar, CInt, peekCString)
import qualified Graphics.X11 as X11
import qualified SDL
import qualified SDL.Font as TTF

type Cell = [Char]

type Color = SDL.V4 Word8

type KeySequence = [Char]

type KeyGrid = [[Cell]]

data Mode = ModeHints | ModeSearch
  deriving (Show, Eq)

data State = State
  { stateGrid :: KeyGrid,
    stateKeySequence :: KeySequence,
    stateIsMatched :: Bool,
    stateIsShiftPressed :: Bool,
    stateIsDragging :: Bool,
    stateRepetition :: Int,
    stateMode :: Mode
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

data OCRMatch = OCRMatch
  { matchStartX :: !CInt,
    matchStartY :: !CInt,
    matchEndX :: !CInt,
    matchEndY :: !CInt,
    matchText :: !String
  }
  deriving (Show)

instance Storable OCRMatch where
  sizeOf _ = 4 * sizeOf (undefined :: CInt) + sizeOf (undefined :: Ptr CChar)

  -- TODO: Remove hardcoding later
  alignment _ = 8

  peek ptr = do
    let cintSize = sizeOf (undefined :: CInt)
    startX <- peek $ castPtr ptr
    startY <- peek $ castPtr ptr `plusPtr` cintSize
    endX <- peek $ castPtr ptr `plusPtr` (2 * cintSize)
    endY <- peek $ castPtr ptr `plusPtr` (3 * cintSize)
    text <- peek $ castPtr ptr `plusPtr` (4 * cintSize)
    textStr <- if text == nullPtr then pure "" else peekCString text
    pure $ OCRMatch startX startY endX endY textStr

  -- NOTE: Dont need poke
  poke _ _ = undefined
