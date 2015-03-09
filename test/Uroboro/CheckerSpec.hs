module Uroboro.CheckerSpec
    (
      spec
    , prelude
    , shouldFail
    ) where

import Test.Hspec
import Text.Parsec (parse)

import Paths_uroboro (getDataFileName)
import Uroboro.Checker
    ( Checker
    , checkerEither
    , checkExp
    , typecheck
    , Context
    , emptyProgram
    , inferExp
    , Program
    )
import Uroboro.Error
import Uroboro.Parser (parseDef, parseExp)
import Uroboro.Tree.Internal (Exp(..), Type(..))
import Utils (parseString)

prelude :: IO Program
prelude = do
    fname <- getDataFileName "samples/prelude.uro"
    input <- readFile fname
    case parse parseDef fname input of
        Left msg -> fail $ "Parser: " ++ show msg
        Right defs -> case checkerEither (typecheck emptyProgram defs) of
            Left _ -> fail "Checker"
            Right p -> return p

-- |Context using prelude
c :: Context
c = [
      ("i", Type "Int")
    , ("f", Type "IntToInt")
    , ("g", Type "TwoIntToInt")
    , ("l", Type "ListOfInt")
    , ("s", Type "StreamOfInt")
    ]

-- |Assert error message
shouldFail :: Show a => Checker a -> String -> Expectation
p `shouldFail` part = case checkerEither p of
  Left (MakeError _ msg) -> msg `shouldContain` part
  Right  x               -> expectationFailure
    ("expected: \"... " ++ part ++ " ...\"\n but got: " ++ show x)

-- |Assert success
successfully :: Checker a -> IO a
successfully p = case checkerEither p of
  -- the "return undefined" is just for the type checker
  -- (expectationFailure will throw an exception)
  Left err -> expectationFailure (show err) >> return undefined
  Right x  -> return x

-- |Assert success
successfully_ :: Checker a -> IO ()
successfully_ p = case checkerEither p of
  -- the "return undefined" is just for the type checker
  -- (expectationFailure will throw an exception)
  Left err -> expectationFailure (show err)
  Right _  -> return ()

spec :: Spec
spec = do
    describe "too few arguments" $ do
        it "constructors" $ do
            p <- prelude
            e <- parseString parseExp "succ()"
            checkExp p [] e (Type "Int") `shouldFail` "Length Mismatch"
        it "calls (data)" $ do
            p <- prelude
            e <- parseString parseExp "map()"
            checkExp p [] e (Type "ListOfInt") `shouldFail` "Length Mismatch"
        it "calls (codata)" $ do
            p <- prelude
            e <- parseString parseExp "mapStream().head()"
            checkExp p [] e (Type "StreamOfInt") `shouldFail` ""
    describe "checkPT (data)" $ do
        it "checks return types" $ do
            defs <- parseString parseDef "data Int where zero(): Float"
            typecheck emptyProgram defs `shouldFail` "Definition Mismatch"
        it "prevents duplicates" $ do
            defs <- parseString parseDef $ unlines
                [ "data Int where zero(): Int"
                , "data Int where succ(): Int"
                ]
            typecheck emptyProgram defs `shouldFail` "Shadowed Definition"
        it "allows data types" $ do
            defs <- parseString parseDef "data Int where zero(): Int"
            successfully_ $ typecheck emptyProgram defs
        it "allows multiple arguments with the same type" $ do
            defs <- parseString parseDef "data A where a(A, A): A"
            successfully_ $ typecheck emptyProgram defs
    describe "checkDef (codata)" $ do
        let stream = "codata StreamOfInt where StreamOfInt.head(): Int"
        it "prevents duplicates" $ do
            defs <- parseString parseDef $ unlines [stream, stream]
            typecheck emptyProgram defs `shouldFail` "Shadowed Definition"
        it "checks argument types" $ do
            defs <- parseString parseDef "codata IntToInt where IntToInt.apply(Int): Int"
            typecheck emptyProgram defs `shouldFail` "Missing Definition"
        it "allows codata types" $ do
            defs <- parseString parseDef stream
            successfully_ $ typecheck emptyProgram defs
        it "allows multiple arguments with the same type" $ do
            defs <- parseString parseDef "codata A where A.a(A, A): A"
            successfully_ $ typecheck emptyProgram defs
    describe "checkExp" $ do
        it "infers construction" $ do
            p <- prelude
            e <- parseString parseExp "empty()"
            t <- successfully $ checkExp p [] e (Type "ListOfInt")
            t `shouldBe` ConExp (Type "ListOfInt") "empty" []
        it "infers applications" $ do
            p <- prelude
            e <- parseString parseExp "map(f, l)"
            t <- successfully $ checkExp p c e (Type "ListOfInt")
            t `shouldBe`
              AppExp (Type "ListOfInt") "map"
                [VarExp (Type "IntToInt") "f", VarExp (Type "ListOfInt") "l"]
    describe "inferPExp" $ do
        it "infers construction" $ do
            p <- prelude
            e <- parseString parseExp "empty()"
            t <- successfully $ inferExp p [] e
            t `shouldBe` ConExp (Type "ListOfInt") "empty" []
        it "infers applications" $ do
            p <- prelude
            e <- parseString parseExp "map(f, l)"
            t <- successfully $ inferExp p c e
            t `shouldBe`
              AppExp (Type "ListOfInt") "map"
                [VarExp (Type "IntToInt") "f", VarExp (Type "ListOfInt") "l"]
