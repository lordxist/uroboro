{-|
Description : Typecheck and run Uroboro code on the command line

This is the executable comprising the user interface to the interpreter.
-}
module Main
    (
      getOpt
    , main
    , Mode(..)
    ) where

import Control.Monad (foldM, forM, void)
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Uroboro.Checker
    ( checkerIO
    , setProgram
    , typecheck
    , inferExp
    , rules)
import Uroboro.Interpreter (eval)
import Uroboro.Parser (parseFile, parseExpression)
import Uroboro.PrettyPrint (render)
import Uroboro.Subtyping (supertypeRelation, SubtypeVariant)
import Uroboro.Tree.External (Def)

-- |How the program operates, and on what data.
data Mode = Help
          | Typecheck [FilePath]
          | Evaluate  [FilePath] String deriving (Eq, Show)

-- |Parse command line options.
getOpt :: [String] -> Mode
getOpt args = case break (== "--") args of
    ([], _)     -> Help
    (x, [])     -> Typecheck x
    (x, [_, y]) -> Evaluate x y
    _           -> Help

-- |For a left value, print it and set the exit code.
eitherIO :: Show a => Either a b -> IO b
eitherIO (Left e)  = do
    print e             -- TODO strip surrounding quotes, or use custom errors
    exitFailure
eitherIO (Right b) = return b

-- |Load libraries.
-- For now, different subtype variants across libaries are not supported (the variant is taken whichever file is parsed first).
parseFiles :: [FilePath] -> IO ([Def], SubtypeVariant)
parseFiles paths = do
    lolAndSv <- forM paths $ \path -> do
      input <- readFile path
      eitherIO $ parseFile path input
    return (concat $ map fst lolAndSv, snd $ lolAndSv !! 0)

-- |Parse given source code, typecheck it, and optionally run it.
-- No output means typechecking was successful.
main :: IO ()
main = do
    args <- getArgs
    case getOpt args of
        Evaluate paths input -> do
            (defs, sv)  <- parseFiles paths
            pexp        <- eitherIO $ parseExpression "command line" input
            let subEnv = supertypeRelation sv defs
            prog        <- checkerIO (typecheck defs) subEnv
            texp        <- flip checkerIO subEnv $ setProgram prog >> inferExp [] pexp

            putStrLn (render $ eval (rules prog) texp)
        Typecheck paths -> do
            (defs, sv)  <- parseFiles paths
            checkerIO (void (typecheck defs)) (supertypeRelation sv defs)
        Help -> do
            putStrLn "USAGE: uroboro FILES [-- EXPRESSION]"
            exitFailure
