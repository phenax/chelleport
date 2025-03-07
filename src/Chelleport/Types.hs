module Chelleport.Types where

import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)
import Data.Default (Default (def))
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

data ModeSearchData = ModeSearchData
  { searchWords :: [OCRMatch],
    searchFilteredWords :: [OCRMatch],
    searchInputText :: String,
    searchHighlightedIndex :: Int
  }
  deriving (Show, Eq)

instance Default ModeSearchData where
  def =
    ModeSearchData
      { searchWords = [],
        searchFilteredWords = [],
        searchInputText = "",
        searchHighlightedIndex = 0
      }

data ModeHintsData = ModeHintsData
  { stateGrid :: KeyGrid,
    stateKeySequence :: KeySequence,
    stateIsMatched :: Bool
  }
  deriving (Show, Eq)

instance Default ModeHintsData where
  def =
    ModeHintsData
      { stateGrid = [],
        stateKeySequence = "",
        stateIsMatched = False
      }

data Mode
  = ModeHints {modeHintsData :: ModeHintsData}
  | ModeSearch {modeSearchData :: ModeSearchData}
  deriving (Show, Eq)

data State = State
  { stateGridRows :: Int,
    stateGridCols :: Int,
    stateIsShiftPressed :: Bool,
    stateIsDragging :: Bool,
    stateRepetition :: Int,
    stateIsModeInitialized :: Bool,
    stateMode :: Mode
  }
  deriving (Show, Eq)

instance Default State where
  def =
    State
      { stateGridRows = 0,
        stateGridCols = 0,
        stateIsShiftPressed = False,
        stateIsDragging = False,
        stateRepetition = 1,
        stateIsModeInitialized = False,
        stateMode = ModeHints def
      }

data Direction = DirUp | DirDown | DirLeft | DirRight
  deriving (Show, Eq)

data AppAction
  = ChainMouseClick MouseButtonType
  | DeleteLastInput
  | HandleFilterInputChange
  | HandleKeyInput SDL.Keycode
  | IncrementHighlightIndex Int
  | IncrementMouseCursor (Int, Int)
  | InitializeMode
  | MouseDragEnd
  | MouseDragStart
  | MouseDragToggle
  | MoveMouseInDirection Direction
  | MoveMousePosition (Int, Int)
  | ResetKeys
  | SetMode Mode
  | ShutdownApp
  | TriggerMouseClick MouseButtonType
  | UpdateRepetition Int
  | UpdateShiftState Bool
  deriving (Show, Eq)

data FontSize = FontSM | FontLG deriving (Show, Eq)

data DrawContext = DrawContext
  { ctxWindow :: SDL.Window,
    ctxRenderer :: SDL.Renderer,
    ctxFontSmall :: TTF.Font,
    ctxFontLarge :: TTF.Font,
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
  deriving (Show, Eq)

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

instance Default OCRMatch where
  def = OCRMatch {matchStartX = 0, matchStartY = 0, matchEndX = 0, matchEndY = 0, matchText = ""}

data Configuration = Configuration
  { configMode :: Mode,
    configShowHelp :: Bool
  }
  deriving (Show, Eq)

instance Default Configuration where
  def = Configuration {configMode = ModeHints def, configShowHelp = False}

data TextAlign = AlignLeft | AlignCenter
  deriving (Show, Eq)

data TextStyle = TextStyle
  { textColor :: Color,
    textSize :: FontSize,
    textAlign :: TextAlign
  }
  deriving (Show, Eq)
