module Specs.ArgsSpec where

import Chelleport.Args (parseArgs)
import Chelleport.Types
import Data.Default (Default (def))
import Test.Hspec

test :: SpecWith ()
test = do
  describe "#parseArgs" $ do
    context "when there are no args" $ do
      it "parses default configuration" $ do
        let config = parseArgs []
        config `shouldBe` Right def

    context "when args contains --help" $ do
      it "enables show help without parsing the rest of the args" $ do
        parseArgs ["--help"] `shouldBe` Right (def {configShowHelp = True})
        parseArgs ["--help", "-m", "mode"] `shouldBe` Right (def {configShowHelp = True})

    context "when args contains -m or --mode with a valid mode" $ do
      it "parses configuration with mode" $ do
        parseArgs ["-m", "search"] `shouldBe` Right (def {configMode = defaultSearchMode})
        parseArgs ["--mode", "search"] `shouldBe` Right (def {configMode = defaultSearchMode})
        parseArgs ["-m", "hints"] `shouldBe` Right (def {configMode = defaultHintsMode})
        parseArgs ["--mode", "hints"] `shouldBe` Right (def {configMode = defaultHintsMode})

    context "when args contains -m or --mode with an invalid mode" $ do
      it "returns with error message" $ do
        parseArgs ["--mode", "invalidmode"] `shouldBe` Left "Invalid mode: invalidmode"
        parseArgs ["-m", "invalidmode"] `shouldBe` Left "Invalid mode: invalidmode"

    context "when args contains -m or --mode without any mode" $ do
      it "returns with error message" $ do
        parseArgs ["--mode"] `shouldBe` Left "Missing value for mode"

    context "when args contains an invalid flag" $ do
      it "enables show help without parsing the rest of the args" $ do
        parseArgs ["--foobar"] `shouldBe` Left "Unrecognized argument: --foobar"
