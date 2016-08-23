module Uroboro.SubtypingSpec
    (
      spec
    ) where

import Control.Monad.Reader

import Test.Hspec

import Uroboro.Parser (parseDef)
import Uroboro.Subtyping
import Uroboro.Tree.External
import Utils (parseString)

spec :: Spec
spec = do
    describe "extends" $ do
        it "recognizes data types directly extending others" $ do
            let source = "data Foo extends Bar where\n\ndata Bar where"
            defs <- parseString parseDef source
            extensionRelation defs (Type "Foo") (Type "Bar") `shouldBe` True
        it "recognizes codata types directly extending others" $ do
            let source = "codata Foo extends Bar where\n\ncodata Bar where"
            defs <- parseString parseDef source
            extensionRelation defs (Type "Foo") (Type "Bar") `shouldBe` True
        it "doesn't falsely recognize non-existing extends relations" $ do
            let source = "codata Foo where\n\ncodata Bar where"
            defs <- parseString parseDef source
            extensionRelation defs (Type "Foo") (Type "Bar") `shouldBe` False
        it "recognizes transitive extends relations" $ do
            let source = "codata Foo extends Bar where\n\ncodata Bar extends Baz where\n\ncodata Baz where"
            defs <- parseString parseDef source
            extensionRelation defs (Type "Foo") (Type "Baz") `shouldBe` True
