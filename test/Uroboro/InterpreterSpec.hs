module Uroboro.InterpreterSpec
    (
      spec
    ) where

import Test.Hspec

import Uroboro.Checker (Program, setProgram, checkerEitherDefault, inferExp, rules)
import Uroboro.CheckerSpec (prelude, uroprog)
import Uroboro.Interpreter (pmatch, eval)
import Uroboro.Parser (parseExp)
import Uroboro.Tree.Internal

import Utils (parseString)

checkWithLib :: String -> IO Program -> IO Exp
checkWithLib input libInput = do
    pexp <- parseString parseExp input
    prog <- libInput
    case checkerEitherDefault (setProgram prog >> inferExp [] pexp) of
        Left msg -> fail $ "Checker:" ++ show msg
        Right texp -> return texp

main :: String -> IO Exp
main input = checkWithLib input prelude

spec :: Spec
spec = do
    let int = Type "Int"
    describe "pattern matching" $ do
        it "fills variables" $ do
            let name = "l"
            let t = Type "ListOfInt"
            let term = (ConExp t "empty" [])
            pmatch term (VarPat t name) `shouldBe` Right [(name, term)]
    describe "eval" $ do
        it "completes" $ do
            p <- fmap rules prelude
            m <- main "add(zero(), succ(zero()))"
            eval p m `shouldBe` ConExp int "succ" [ConExp int "zero" []]
        it "can run add1(0)" $ do
            p <- fmap rules prelude
            m <- main "add1().apply1(zero())"
            r <- main "succ(zero())"
            eval p m `shouldBe` r
        it "can run add1(1)" $ do
            p <- fmap rules prelude
            m <- main "add1().apply1(succ(zero()))"
            r <- main "succ(succ(zero()))"
            eval p m `shouldBe` r
        it "matches manual reduction" $ do
            p <- fmap rules prelude
            m <- main "map(add1(), cons(succ(zero()), cons(zero(), empty())))"
            r <- main "cons(succ(succ(zero())), cons(succ(zero()), empty()))"
            eval p m `shouldBe` r
        it "is compatible with simple type conversion" $ do
            let prog = uroprog "type-conversion"
            p <- fmap rules prog
            m <- checkWithLib "subToParentVia(sub())" prog
            r <- checkWithLib "sub()" prog
            eval p m `shouldBe` r
    describe "prelude" $ do
        it "adder works" $ do
            p <- fmap rules prelude
            m <- main "map(adder(succ(zero())), cons(succ(zero()), cons(zero(), empty())))"
            r <- main "cons(succ(succ(zero())), cons(succ(zero()), empty()))"
            eval p m `shouldBe` r
