module Specs.AppEventSpec where

import Chelleport (eventHandler)
import Chelleport.Types
import qualified SDL
import SDL.Internal.Numbered (FromNumber (fromNumber))
import Test.Hspec
import Unsafe.Coerce (unsafeCoerce)

test :: SpecWith ()
test = do
  describe "#eventHandler" $ do
    let mkEvent payload = SDL.Event {SDL.eventTimestamp = 0, SDL.eventPayload = payload}
    let mkKeyboardEvent key motion =
          mkEvent $
            SDL.KeyboardEvent $
              SDL.KeyboardEventData
                { SDL.keyboardEventWindow = unsafeCoerce (0 :: Integer),
                  SDL.keyboardEventRepeat = False,
                  SDL.keyboardEventKeysym =
                    SDL.Keysym
                      { SDL.keysymScancode = SDL.Scancode0,
                        SDL.keysymModifier = fromNumber 0,
                        SDL.keysymKeycode = key
                      },
                  SDL.keyboardEventKeyMotion = motion
                }

    context "when window quit event is triggered" $ do
      it "shuts down app" $ do
        let action = eventHandler $ mkEvent SDL.QuitEvent
        action `shouldBe` Just ShutdownApp

    context "when escape key is pressed" $ do
      it "shuts down app" $ do
        let action = eventHandler $ mkKeyboardEvent SDL.KeycodeEscape SDL.Pressed
        action `shouldBe` Just ShutdownApp

    context "when space key is pressed" $ do
      it "triggers left mouse button click" $ do
        let action = eventHandler $ mkKeyboardEvent SDL.KeycodeSpace SDL.Pressed
        action `shouldBe` Just TriggerLeftClick

    context "when tab key is pressed" $ do
      it "resets key state" $ do
        let action = eventHandler $ mkKeyboardEvent SDL.KeycodeTab SDL.Pressed
        action `shouldBe` Just ResetKeys

    context "when an alphanumeric key (excluding Q) is pressed" $ do
      it "calls key input handler" $ do
        eventHandler (mkKeyboardEvent SDL.KeycodeA SDL.Pressed) `shouldBe` Just (HandleKeyInput SDL.KeycodeA)
        eventHandler (mkKeyboardEvent SDL.KeycodeQ SDL.Pressed) `shouldBe` Just (HandleKeyInput SDL.KeycodeQ)
        eventHandler (mkKeyboardEvent SDL.Keycode9 SDL.Pressed) `shouldBe` Just (HandleKeyInput SDL.Keycode9)

    context "when shift key is pressed" $ do
      it "enables shift" $ do
        let action = eventHandler $ mkKeyboardEvent SDL.KeycodeRShift SDL.Pressed
        action `shouldBe` Just (UpdateShiftState True)

    context "when shift key is released" $ do
      it "disabled shift" $ do
        let action = eventHandler $ mkKeyboardEvent SDL.KeycodeRShift SDL.Released
        action `shouldBe` Just (UpdateShiftState False)
