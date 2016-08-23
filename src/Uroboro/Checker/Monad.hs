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
    , checkerEitherDefault
    , checkerIO
    , checkerIODefault
      -- * Checker state
    , Program (..)
    , emptyProgram
    , getProgram
    , setProgram
      -- * Checker failure
    , failAt
    , TypeSubEnv
    ) where

import Control.Applicative
import Control.Monad.Reader

import System.Exit (exitFailure)

import Uroboro.Error (Error (MakeError), Location)

import qualified Uroboro.Tree.Internal as Int
import qualified Uroboro.Tree.External as Ext

-- |Basic checker monad.
newtype BasicChecker a = BasicChecker {
    runBasicChecker :: forall r . Program -> (Error -> r) -> (Program -> a -> r) -> r
  }

instance Functor BasicChecker where
  fmap f p = BasicChecker (\s e k -> runBasicChecker p s e (\s' a -> k s' (f a)))

instance Applicative BasicChecker where
  pure a = BasicChecker (\s _ k -> k s a)
  p <*> q =
    BasicChecker (\s e k ->
      runBasicChecker p s e (\s' f ->
        runBasicChecker q s' e (\s'' a ->
          k s'' (f a))))

instance Monad BasicChecker where
  return a = BasicChecker (\s _ k -> k s a)
  p >>= f =
    BasicChecker (\s e k ->
      runBasicChecker p s e (\s' a ->
      runBasicChecker (f a) s' e k))

-- |On error, return left (basic checker).
basicCheckerEither :: BasicChecker a -> Either Error a
basicCheckerEither p = runBasicChecker p emptyProgram Left (\_ a -> Right a)

-- |On error, print it and set the exit code (basic checker).
basicCheckerIO :: BasicChecker a -> IO a
basicCheckerIO p = runBasicChecker p emptyProgram
  (\e -> print e >> exitFailure)
  (\_ a -> return a)

-- |Fail the monad, but with location (basic checker).
failAt_ :: Location -> String -> BasicChecker a
failAt_ location message = BasicChecker (\_ e _ -> e (MakeError location message))

-- |State of the typechecker.
data Program = Program {
      typeNames    :: [Int.Type]  -- Cache types of constructors and destructors.
    , constructors :: [Ext.ConSig]
    , destructors  :: [Ext.DesSig]
    , functions    :: [Ext.FunSig] -- Always update functions and rules together.
    -- |Extract the end result of typechecking.
    , rules        :: Int.Rules
    } deriving (Show)

-- |Get program (basic checker).
getProgram_ :: BasicChecker Program
getProgram_ = BasicChecker (\s _ k -> k s s)

-- |Set program (basic checker).
setProgram_ :: Program -> BasicChecker ()
setProgram_ s = BasicChecker (\_ _ k -> k s ())

-- |Start value for folds.
emptyProgram :: Program
emptyProgram = Program [] [] [] [] []

-- |Environment where a certain type subsumption relation holds.
type TypeSubEnv = Ext.Type -> Ext.Type -> Bool

-- |Checker monad, generalized to allow for arbitrary type subsumption environments. (Can be used for subtyping.)
type Checker a = ReaderT TypeSubEnv BasicChecker a

convert :: (BasicChecker a -> b) -> Checker a -> TypeSubEnv -> b
convert f p e = f $ runReaderT p e

-- |On error, return left.
checkerEither :: Checker a -> TypeSubEnv -> Either Error a
checkerEither = convert basicCheckerEither

-- |Like checkerEither, but default the subsumption environment to equality.
checkerEitherDefault :: Checker a -> Either Error a
checkerEitherDefault p = checkerEither p (==)

-- |On error, print it and set the exit code.
checkerIO :: Checker a -> TypeSubEnv -> IO a
checkerIO = convert basicCheckerIO

-- |Like checkerIO, but default the subsumption environment to equality.
checkerIODefault :: Checker a -> IO a
checkerIODefault p = checkerIO p (==)

-- |Fail the monad, but with location.
failAt :: Location -> String -> Checker a
failAt location message = lift $ failAt_ location message

-- |Get program.
getProgram :: Checker Program
getProgram = lift getProgram_

-- |Set program.
setProgram :: Program -> Checker ()
setProgram = lift . setProgram_
