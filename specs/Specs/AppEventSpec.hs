module Specs.AppEventSpec where

import Chelleport (eventHandler)
import Chelleport.Types
import Data.Default (Default (def))
import qualified SDL
import SDL.Internal.Numbered (FromNumber (fromNumber))
import Test.Hspec
import Unsafe.Coerce (unsafeCoerce)

test :: SpecWith ()
test = do
  describe "#eventHandler currentState" $ do
    let mkEvent payload = SDL.Event {SDL.eventTimestamp = 0, SDL.eventPayload = payload}
    let mkKeyboardEvent key motion modifier =
          mkEvent $
            SDL.KeyboardEvent $
              SDL.KeyboardEventData
                { SDL.keyboardEventWindow = unsafeCoerce (0 :: Integer),
                  SDL.keyboardEventRepeat = False,
                  SDL.keyboardEventKeysym =
                    SDL.Keysym
                      { SDL.keysymScancode = SDL.Scancode0,
                        SDL.keysymModifier = modifier,
                        SDL.keysymKeycode = key
                      },
                  SDL.keyboardEventKeyMotion = motion
                }
    let defaultMod = fromNumber 0
    let currentState = def

    context "when window quit event is triggered" $ do
      it "shuts down app" $ do
        let action = eventHandler currentState $ mkEvent SDL.QuitEvent
        action `shouldBe` Just ShutdownApp

    context "when escape key is pressed" $ do
      it "shuts down app" $ do
        let action = eventHandler currentState $ mkKeyboardEvent SDL.KeycodeEscape SDL.Pressed defaultMod
        action `shouldBe` Just ShutdownApp

    context "when ctrl+v is pressed" $ do
      it "toggles dragging" $ do
        let action = eventHandler currentState $ mkKeyboardEvent SDL.KeycodeV SDL.Pressed (defaultMod {SDL.keyModifierLeftCtrl = True})
        action `shouldBe` Just MouseDragToggle

    context "when space key is pressed" $ do
      it "triggers left mouse button click" $ do
        let action = eventHandler currentState $ mkKeyboardEvent SDL.KeycodeSpace SDL.Pressed defaultMod
        action `shouldBe` Just (TriggerMouseClick LeftClick)

      context "when pressed with right shift" $ do
        it "chains left mouse button click" $ do
          let action = eventHandler currentState $ mkKeyboardEvent SDL.KeycodeSpace SDL.Pressed (defaultMod {SDL.keyModifierRightShift = True})
          action `shouldBe` Just (ChainMouseClick LeftClick)

      context "when pressed with left shift" $ do
        it "chains left mouse button click" $ do
          let action = eventHandler currentState $ mkKeyboardEvent SDL.KeycodeSpace SDL.Pressed (defaultMod {SDL.keyModifierLeftShift = True})
          action `shouldBe` Just (ChainMouseClick LeftClick)

    context "when enter key is pressed" $ do
      it "triggers left mouse button click" $ do
        let action = eventHandler currentState $ mkKeyboardEvent SDL.KeycodeReturn SDL.Pressed defaultMod
        action `shouldBe` Just (TriggerMouseClick LeftClick)

      context "when pressed with right shift" $ do
        it "chains left mouse button click" $ do
          let action = eventHandler currentState $ mkKeyboardEvent SDL.KeycodeReturn SDL.Pressed (defaultMod {SDL.keyModifierRightShift = True})
          action `shouldBe` Just (ChainMouseClick LeftClick)

      context "when pressed with left shift" $ do
        it "chains left mouse button click" $ do
          let action = eventHandler currentState $ mkKeyboardEvent SDL.KeycodeReturn SDL.Pressed (defaultMod {SDL.keyModifierLeftShift = True})
          action `shouldBe` Just (ChainMouseClick LeftClick)

    context "when minus key is pressed" $ do
      it "triggers left mouse button click" $ do
        let action = eventHandler currentState $ mkKeyboardEvent SDL.KeycodeMinus SDL.Pressed defaultMod
        action `shouldBe` Just (TriggerMouseClick RightClick)

      context "when pressed with right shift" $ do
        it "chains right mouse button click" $ do
          let action = eventHandler currentState $ mkKeyboardEvent SDL.KeycodeMinus SDL.Pressed (defaultMod {SDL.keyModifierRightShift = True})
          action `shouldBe` Just (ChainMouseClick RightClick)

      context "when pressed with left shift" $ do
        it "chains right mouse button click" $ do
          let action = eventHandler currentState $ mkKeyboardEvent SDL.KeycodeMinus SDL.Pressed (defaultMod {SDL.keyModifierLeftShift = True})
          action `shouldBe` Just (ChainMouseClick RightClick)

    context "when backspace key is pressed" $ do
      it "resets key state" $ do
        let action = eventHandler currentState $ mkKeyboardEvent SDL.KeycodeBackspace SDL.Pressed defaultMod
        action `shouldBe` Just ResetKeys

    context "when an alphanumeric key (excluding Q) is pressed" $ do
      it "calls key input handler" $ do
        eventHandler currentState (mkKeyboardEvent SDL.KeycodeA SDL.Pressed defaultMod) `shouldBe` Just (HandleKeyInput SDL.KeycodeA)
        eventHandler currentState (mkKeyboardEvent SDL.KeycodeQ SDL.Pressed defaultMod) `shouldBe` Just (HandleKeyInput SDL.KeycodeQ)

    context "when shift key is pressed" $ do
      it "enables shift" $ do
        let action = eventHandler currentState $ mkKeyboardEvent SDL.KeycodeRShift SDL.Pressed defaultMod
        action `shouldBe` Just (UpdateShiftState True)

    context "when shift key is released" $ do
      it "disabled shift" $ do
        let action = eventHandler currentState $ mkKeyboardEvent SDL.KeycodeRShift SDL.Released defaultMod
        action `shouldBe` Just (UpdateShiftState False)

    context "when digit is pressed" $ do
      it "sets repetition count" $ do
        let action = eventHandler currentState $ mkKeyboardEvent SDL.Keycode9 SDL.Pressed defaultMod
        action `shouldBe` Just (UpdateRepetition 9)
