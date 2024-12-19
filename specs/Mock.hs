module Mock where

import Chelleport.AppShell (MonadAppShell (..))
import Chelleport.Control (MonadControl (..))
import Chelleport.Draw (MonadDraw (..))
import Chelleport.Types
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState (get, state), StateT (runStateT), gets)
import Data.Text (Text)
import Foreign.C (CInt)

data Call
  = CallPressMouseButton MouseButtonType
  | CallMoveMousePosition CInt CInt
  | CallGetMousePointerPosition
  | CallHideWindow
  | CallShowWindow
  | CallShutdownApp
  | CallDrawLine (CInt, CInt) (CInt, CInt)
  | CallDrawText (CInt, CInt) Color Text
  | CallDrawCircle Int (CInt, CInt)
  | CallSetDrawColor Color
  | CallWindowSize
  deriving (Show, Eq)

newtype MockCalls = MockCalls {calls :: [Call]}

registerMockCall :: (MonadState MockCalls m) => Call -> m ()
registerMockCall call =
  void $ state (\mock -> ((), MockCalls {calls = calls mock ++ [call]}))

runWithMocks :: (MonadIO m) => TestM m a -> m (a, MockCalls)
runWithMocks action = runStateT (runTestM action) (MockCalls [])

newtype TestM m a = TestM {runTestM :: StateT MockCalls m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadState MockCalls)

instance (MonadIO m) => MonadControl (TestM m) where
  pressMouseButton btn = registerMockCall $ CallPressMouseButton btn
  moveMousePointer x y = registerMockCall $ CallMoveMousePosition x y
  getMousePointerPosition = (0, 0) <$ registerMockCall CallGetMousePointerPosition

instance (MonadIO m) => MonadDraw (TestM m) where
  drawLine p1 p2 = registerMockCall $ CallDrawLine p1 p2
  drawText p color text = (0, 0) <$ registerMockCall (CallDrawText p color text)
  drawCircle radius p = registerMockCall $ CallDrawCircle radius p
  setDrawColor color = registerMockCall $ CallSetDrawColor color
  windowSize = 0 <$ registerMockCall CallWindowSize

instance (MonadIO m) => MonadAppShell (TestM m) where
  hideWindow = registerMockCall CallHideWindow
  showWindow = registerMockCall CallShowWindow
  shutdownApp = registerMockCall CallShutdownApp
