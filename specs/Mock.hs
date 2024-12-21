module Mock where

import Chelleport.AppShell (MonadAppShell (..))
import Chelleport.Control (MonadControl (..))
import Chelleport.Draw (MonadDraw (..))
import Chelleport.Types
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState (state), StateT (runStateT))
import Data.Text (Text)
import Foreign.C (CInt)
import Test.Hspec

data Call
  = CallDrawCircle Int (CInt, CInt)
  | CallDrawLine (CInt, CInt) (CInt, CInt)
  | CallDrawText (CInt, CInt) Color Text
  | CallGetMousePointerPosition
  | CallHideWindow
  | CallMouseButtonDown
  | CallMouseButtonUp
  | CallMoveMousePosition CInt CInt
  | CallPressMouseButton MouseButtonType
  | CallSetDrawColor Color
  | CallShowWindow
  | CallShutdownApp
  | CallWindowPosition
  | CallWindowSize
  deriving (Show, Eq)

newtype MockCalls = MockCalls {calls :: [Call]}
  deriving (Show)

registerMockCall :: (MonadState MockCalls m) => Call -> m ()
registerMockCall call =
  void $ state (\mock -> ((), MockCalls {calls = calls mock ++ [call]}))

shouldHaveCalled :: (HasCallStack) => MockCalls -> Call -> Expectation
shouldHaveCalled mock call = calls mock `shouldContain` [call]

runWithMocks :: (MonadIO m) => TestM m a -> m (a, MockCalls)
runWithMocks action = runStateT (runTestM action) (MockCalls [])

newtype TestM m a = TestM {runTestM :: StateT MockCalls m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadState MockCalls)

instance (MonadIO m) => MonadControl (TestM m) where
  pressMouseButton btn = registerMockCall $ CallPressMouseButton btn
  moveMousePointer x y = registerMockCall $ CallMoveMousePosition x y
  getMousePointerPosition = (42, 42) <$ registerMockCall CallGetMousePointerPosition
  mouseButtonDown = registerMockCall CallMouseButtonDown
  mouseButtonUp = registerMockCall CallMouseButtonUp

mockWindowWidth :: CInt
mockWindowWidth = 1920

mockWindowHeight :: CInt
mockWindowHeight = 1080

mockWindowOffsetX :: CInt
mockWindowOffsetX = 200

mockWindowOffsetY :: CInt
mockWindowOffsetY = 100

instance (MonadIO m) => MonadDraw (TestM m) where
  drawLine p1 p2 = registerMockCall $ CallDrawLine p1 p2
  drawText p color text = (0, 0) <$ registerMockCall (CallDrawText p color text)
  drawCircle radius p = registerMockCall $ CallDrawCircle radius p
  setDrawColor color = registerMockCall $ CallSetDrawColor color
  windowSize = (mockWindowWidth, mockWindowHeight) <$ registerMockCall CallWindowSize
  windowPosition = (mockWindowOffsetX, mockWindowOffsetY) <$ registerMockCall CallWindowPosition

instance (MonadIO m) => MonadAppShell (TestM m) where
  hideWindow = registerMockCall CallHideWindow
  showWindow = registerMockCall CallShowWindow
  shutdownApp = registerMockCall CallShutdownApp
