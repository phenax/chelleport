module TestUtils where

import Chelleport.AppShell (MonadAppShell (..))
import Chelleport.Control (MonadControl (..))
import Chelleport.Draw (MonadDraw (..))
import Chelleport.OCR (MonadOCR (..))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState (state), StateT (runStateT))
import qualified Data.Text as Text
import Foreign.C (CInt)
import Mock
import Test.Hspec

$(generateMock [''MonadDraw, ''MonadControl, ''MonadAppShell, ''MonadOCR])

newtype MockCalls = MockCalls {calls :: [Call]}
  deriving (Show)

shouldHaveCalled :: (HasCallStack) => MockCalls -> Call -> Expectation
shouldHaveCalled mock call = calls mock `shouldContain` [call]

runWithMocks :: (MonadIO m) => TestM m a -> m (a, MockCalls)
runWithMocks action = runStateT (runTestM action) (MockCalls [])

newtype TestM m a = TestM {runTestM :: StateT MockCalls m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadState MockCalls)

mockWindowWidth :: CInt
mockWindowWidth = 1920

mockWindowHeight :: CInt
mockWindowHeight = 1080

mockWindowOffsetX :: CInt
mockWindowOffsetX = 200

mockWindowOffsetY :: CInt
mockWindowOffsetY = 100

mockTextWidth :: Int
mockTextWidth = 10

registerMockCall :: (MonadState MockCalls m) => Call -> m ()
registerMockCall call =
  void $ state (\mock -> ((), MockCalls {calls = calls mock ++ [call]}))

instance (MonadIO m) => MonadControl (TestM m) where
  clickMouseButton btn = registerMockCall $ Mock_clickMouseButton btn
  moveMousePointer x y = registerMockCall $ Mock_moveMousePointer x y
  getMousePointerPosition = (42, 42) <$ registerMockCall Mock_getMousePointerPosition
  pressMouseButton = registerMockCall Mock_pressMouseButton
  releaseMouseButton = registerMockCall Mock_releaseMouseButton

instance (MonadIO m) => MonadDraw (TestM m) where
  drawLine p1 p2 = registerMockCall $ Mock_drawLine p1 p2
  fillRect p size = registerMockCall $ Mock_fillRect p size
  drawText p color text = (fromIntegral $ mockTextWidth * Text.length text, 0) <$ registerMockCall (Mock_drawText p color text)
  drawCircle radius p = registerMockCall $ Mock_drawCircle radius p
  setDrawColor color = registerMockCall $ Mock_setDrawColor color
  windowSize = (mockWindowWidth, mockWindowHeight) <$ registerMockCall Mock_windowSize
  windowPosition = (mockWindowOffsetX, mockWindowOffsetY) <$ registerMockCall Mock_windowPosition

instance (MonadIO m) => MonadAppShell (TestM m) where
  hideWindow = registerMockCall Mock_hideWindow
  showWindow = registerMockCall Mock_showWindow
  shutdownApp = registerMockCall Mock_shutdownApp

instance (MonadIO m) => MonadOCR (TestM m) where
  getWordsOnScreen = [] <$ registerMockCall Mock_getWordsOnScreen
