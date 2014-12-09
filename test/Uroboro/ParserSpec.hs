module Uroboro.ParserSpec
    (
      spec
    ) where

import Data.Either (isRight)
import Text.Parsec (parse)

import Test.Hspec

import Uroboro.Parser
import Uroboro.Tree

import Paths_uroboro
import Utils()

spec :: Spec
spec = do
    describe "parser" $ do
        it "recognizes the prelude" $ do
            fname <- getDataFileName "samples/prelude.uro"
            input <- readFile fname
            parse parseDef fname input `shouldSatisfy` isRight
        it "observes argument order (constructor)" $ do
            let source = "data Int where zero(): Int"
            let parsed = PTPos "Int" [PTCon "Int" "zero" []]
            parse parseDef "" source `shouldBe` Right [parsed]
        it "observes argument order (destructor)" $ do
            let source = "codata StreamOfInt where StreamOfInt.head(): Int"
            let parsed = PTNeg "StreamOfInt" [PTDes "Int" "head" [] "StreamOfInt"]
            parse parseDef "" source `shouldBe` Right [parsed]
    describe "command line" $ do
        it "ignores whitespace" $ do
            parse parseExp "" "  x  " `shouldBe` Right (PVar "x")
        it "uses the longest match" $ do
            parse parseExp "" "x()" `shouldBe` Right (PApp "x" [])
