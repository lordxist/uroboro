{-# LANGUAGE RankNTypes #-}

{-|
Description : Checker monad.

The 'Checker' monad used in the static checking passes of the
Uroboro implementation.

This monad provides access to a state of type 'Program' and
allows to abort computation with an 'Error' value.
-}
module Uroboro.Checker.Monad
    ( -- * Type checking monad
      Checker
      -- * Running the checker
    , checkerEither
    , checkerIO
      -- * Checker state
    , Program (..)
    , emptyProgram
    , getProgram
    , setProgram
      -- * Checker failure
    , failAt
    ) where

import Control.Applicative

import System.Exit (exitFailure)

import Uroboro.Error (Error (MakeError), Location)

import qualified Uroboro.Tree.Internal as Int
import qualified Uroboro.Tree.External as Ext

-- |Checker monad.
newtype Checker a = Checker {
    runChecker :: forall r . Program -> (Error -> r) -> (Program -> a -> r) -> r
  }

instance Functor Checker where
  fmap f p = Checker (\s e k -> runChecker p s e (\s' a -> k s' (f a)))

instance Applicative Checker where
  pure a = Checker (\s _ k -> k s a)
  p <*> q =
    Checker (\s e k ->
      runChecker p s e (\s' f ->
        runChecker q s' e (\s'' a ->
          k s'' (f a))))

instance Monad Checker where
  return a = Checker (\s _ k -> k s a)
  p >>= f =
    Checker (\s e k ->
      runChecker p s e (\s' a ->
      runChecker (f a) s' e k))

-- |On error, return left.
checkerEither :: Checker a -> Either Error a
checkerEither p = runChecker p emptyProgram Left (\_ a -> Right a)

-- |On error, print it and set the exit code.
checkerIO :: Checker a -> IO a
checkerIO p = runChecker p emptyProgram
  (\e -> print e >> exitFailure)
  (\_ a -> return a)

-- |Fail the monad, but with location.
failAt :: Location -> String -> Checker a
failAt location message = Checker (\_ e _ -> e (MakeError location message))

-- |State of the typechecker.
data Program = Program {
      typeNames    :: [Int.Type]  -- Cache types of constructors and destructors.
    , constructors :: [Ext.ConSig]
    , destructors  :: [Ext.DesSig]
    , functions    :: [Ext.FunSig] -- Always update functions and rules together.
    -- |Extract the end result of typechecking.
    , rules        :: Int.Rules
    } deriving (Show)

-- |Get program.
getProgram :: Checker Program
getProgram = Checker (\s _ k -> k s s)

-- |Set program.
setProgram :: Program -> Checker ()
setProgram s = Checker (\_ _ k -> k s ())

-- |Start value for folds.
emptyProgram :: Program
emptyProgram = Program [] [] [] [] []
