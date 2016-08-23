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
    , checkerEitherDefault
    , setProgram
    , checkExp
    , typecheck
    , Context
    , inferExp
    , Program
    , TypeSubEnv
    )
import Uroboro.Error
import Uroboro.Parser (parseDef, parseExp)
import Uroboro.Subtyping
import Uroboro.Tree.External(Def)
import Uroboro.Tree.Internal (Exp(..), Type(..))
import Utils (parseString)

urofile :: String -> IO [Def]
urofile s = do
    fname <- getDataFileName $ "samples/" ++ s ++ ".uro"
    input <- readFile fname
    case parse parseDef fname input of
        Left msg -> fail $ "Parser: " ++ show msg
        Right defs -> return defs

prelude :: IO Program
prelude = do
    defs <- urofile "prelude"
    case checkerEitherDefault (typecheck defs) of
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
p `shouldFail` part = case checkerEitherDefault p of
  Left (MakeError _ msg) -> msg `shouldContain` part
  Right  x               -> expectationFailure
    ("expected: \"... " ++ part ++ " ...\"\n but got: " ++ show x)

-- |Assert success
successfully :: Checker a -> IO a
successfully p = case checkerEitherDefault p of
  -- the "return undefined" is just for the type checker
  -- (expectationFailure will throw an exception)
  Left err -> expectationFailure (show err) >> return undefined
  Right x  -> return x

-- |Assert success
successfully_ :: Checker a -> IO ()
successfully_ p = case checkerEitherDefault p of
  -- the "return undefined" is just for the type checker
  -- (expectationFailure will throw an exception)
  Left err -> expectationFailure (show err)
  Right _  -> return ()

-- |Assert success in type subsumption environment
successfullyInEnv_ :: Checker a -> TypeSubEnv -> IO ()
successfullyInEnv_ p env = case checkerEither p env of
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
            (setProgram p >> checkExp [] e (Type "Int")) `shouldFail` "Length Mismatch"
        it "calls (data)" $ do
            p <- prelude
            e <- parseString parseExp "map()"
            (setProgram p >> checkExp [] e (Type "ListOfInt")) `shouldFail` "Length Mismatch"
        it "calls (codata)" $ do
            p <- prelude
            e <- parseString parseExp "mapStream().head()"
            (setProgram p >> checkExp [] e (Type "StreamOfInt")) `shouldFail` ""
    describe "checkPT (data)" $ do
        it "checks return types" $ do
            defs <- parseString parseDef "data Int where zero(): Float"
            typecheck defs `shouldFail` "Definition Mismatch"
        it "prevents duplicates" $ do
            defs <- parseString parseDef $ unlines
                [ "data Int where zero(): Int"
                , "data Int where succ(): Int"
                ]
            typecheck defs `shouldFail` "Shadowed Definition"
        it "allows data types" $ do
            defs <- parseString parseDef "data Int where zero(): Int"
            successfully_ $ typecheck defs
        it "allows multiple arguments with the same type" $ do
            defs <- parseString parseDef "data A where a(A, A): A"
            successfully_ $ typecheck defs
    describe "checkDef (codata)" $ do
        let stream = "codata StreamOfInt where StreamOfInt.head(): Int"
        it "prevents duplicates" $ do
            defs <- parseString parseDef $ unlines [stream, stream]
            typecheck defs `shouldFail` "Shadowed Definition"
        it "checks argument types" $ do
            defs <- parseString parseDef "codata IntToInt where IntToInt.apply(Int): Int"
            typecheck defs `shouldFail` "Missing Definition"
        it "allows codata types" $ do
            defs <- parseString parseDef stream
            successfully_ $ typecheck defs
        it "allows multiple arguments with the same type" $ do
            defs <- parseString parseDef "codata A where A.a(A, A): A"
            successfully_ $ typecheck defs
    describe "checkExp" $ do
        it "infers construction" $ do
            p <- prelude
            e <- parseString parseExp "empty()"
            t <- successfully $ setProgram p >> checkExp [] e (Type "ListOfInt")
            t `shouldBe` ConExp (Type "ListOfInt") "empty" []
        it "infers applications" $ do
            p <- prelude
            e <- parseString parseExp "map(f, l)"
            t <- successfully $ setProgram p >> checkExp c e (Type "ListOfInt")
            t `shouldBe`
              AppExp (Type "ListOfInt") "map"
                [VarExp (Type "IntToInt") "f", VarExp (Type "ListOfInt") "l"]
    describe "inferPExp" $ do
        it "infers construction" $ do
            p <- prelude
            e <- parseString parseExp "empty()"
            t <- successfully $ setProgram p >> inferExp [] e
            t `shouldBe` ConExp (Type "ListOfInt") "empty" []
        it "infers applications" $ do
            p <- prelude
            e <- parseString parseExp "map(f, l)"
            t <- successfully $ setProgram p >> inferExp c e
            t `shouldBe`
              AppExp (Type "ListOfInt") "map"
                [VarExp (Type "IntToInt") "f", VarExp (Type "ListOfInt") "l"]
    describe "subtypes" $ do
        it "allows data subtypes" $ do
            defs <- urofile "data-subtypes"
            successfullyInEnv_ (typecheck defs) (extensionRelation defs)
        it "allows codata subtypes" $ do
            defs <- urofile "codata-subtypes"
            successfullyInEnv_ (typecheck defs) (extensionRelation defs)


