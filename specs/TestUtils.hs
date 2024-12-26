module TestUtils where

import Chelleport.AppShell (MonadAppShell (..))
import Chelleport.Control (MonadControl (..))
import Chelleport.Draw (MonadDraw (..))
import Chelleport.OCR (MonadOCR (..))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (StateT (runStateT), gets)
import Data.Typeable (cast)
import Foreign.C (CInt)
import Mock
import Unsafe.Coerce (unsafeCoerce)

$(generateMock [''MonadDraw, ''MonadControl, ''MonadAppShell, ''MonadOCR])

mockWindowWidth :: CInt
mockWindowWidth = 1920

mockWindowHeight :: CInt
mockWindowHeight = 1080

mockWindowSize :: (CInt, CInt)
mockWindowSize = (mockWindowWidth, mockWindowHeight)

mockWindowOffsetX :: CInt
mockWindowOffsetX = 200

mockWindowOffsetY :: CInt
mockWindowOffsetY = 100

mockWindowPosition :: (CInt, CInt)
mockWindowPosition = (mockWindowOffsetX, mockWindowOffsetY)

mockTextWidth :: Int
mockTextWidth = 10

runWithMocks :: (MonadIO m) => TestM x m a -> m (a, MockCalls)
runWithMocks act = runTestMWithMocks $ do
  -- Default mocks
  Mock_windowSize `mockReturns` mockWindowSize
  Mock_windowPosition `mockReturns` mockWindowPosition
  act
