{-# LANGUAGE RankNTypes #-}

{-|
Description : Typechecker

Typecheck parser output, which turns it into interpreter input.
-}
module Uroboro.Checker
    ( -- * Type checking monad
      Checker
    , checkerEither
    , checkerIO
      -- * Type checking
    , checkExp
    , Context
    , setProgram
    , inferExp
    , Program
    , rules
    , typecheck
    ) where

import Control.Applicative
import Control.Monad (when, zipWithM)
import Data.List ((\\), find, nub, nubBy)
import Data.Foldable (traverse_)

import System.Exit (exitFailure)

import Uroboro.Error (Error (MakeError), Location)

import Uroboro.Tree.Common (Identifier)
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

-- |On error, print it and set the exit code.
checkerIO :: Checker a -> IO a
checkerIO p = runChecker p emptyProgram
  (\e -> print e >> exitFailure)
  (\_ a -> return a)

-- |On error, return left.
checkerEither :: Checker a -> Either Error a
checkerEither p = runChecker p emptyProgram Left (\_ a -> Right a)

-- |Fail the monad, but with location.
failAt :: Location -> String -> Checker a
failAt location message = Checker (\_ e _ -> e (MakeError location message))

-- |Get program.
getProgram :: Checker Program
getProgram = Checker (\s _ k -> k s s)

-- |Set program.
setProgram :: Program -> Checker ()
setProgram s = Checker (\_ _ k -> k s ())

-- |Signature of a function definition.
type FunSig = (Identifier, (Location, [Int.Type], Int.Type))

-- |State of the typechecker.
data Program = Program {
      typeNames    :: [Int.Type]  -- Cache types of constructors and destructors.
    , constructors :: [Ext.ConSig]
    , destructors  :: [Ext.DesSig]
    , functions    :: [FunSig] -- Always update functions and rules together.
    -- |Extract the end result of typechecking.
    , rules        :: Int.Rules
    } deriving (Show)

-- |Start value for folds.
emptyProgram :: Program
emptyProgram = Program [] [] [] [] []

-- |Check that a constructor exists and return its signature.
inferCon :: Location -> Identifier -> Checker Ext.ConSig
inferCon loc name = do
  p <- getProgram
  case find match (constructors p) of
    Just consig -> return consig
    Nothing -> failAt loc "Missing Definition"
  where
    match sig = Ext.name sig == name

-- |Check that a constructor for a given type exists and return its signature.
checkCon :: Location -> Identifier -> Int.Type -> Checker Ext.ConSig
checkCon loc name t = do
  consig <- inferCon loc name
  when (Ext.returnType consig /= t) $ do
    failAt loc "Missing Definition"
  return consig

-- |Check that a destructor exists and return its signature.
inferDes :: Location -> Identifier -> Checker Ext.DesSig
inferDes loc name = do
  p <- getProgram
  case find match (destructors p) of
    Just dessig -> return dessig
    Nothing -> failAt loc "Missing Definition"
  where
    match sig = Ext.name sig == name

-- |Check that a destructor (with given codomain) exists and return its signature.
checkDes :: Location -> Identifier -> Int.Type -> Checker Ext.DesSig
checkDes loc name t = do
  dessig <- inferDes loc name
  when (Ext.returnType dessig /= t) $ do
    failAt loc "Missing Definition"
  return dessig

data FunOrConSig
  = Fun FunSig
  | Con Ext.ConSig

instance Ext.HasReturnType FunOrConSig where
  returnType (Fun (_, (_, _, t))) = t
  returnType (Con conSig) = Ext.returnType conSig

instance Ext.HasArgumentTypes FunOrConSig where
  argumentTypes (Fun (_, (_, ts, _))) = ts
  argumentTypes (Con conSig) = Ext.argumentTypes conSig

instance Ext.HasLocation FunOrConSig where
  location (Fun (_, (loc, _, _))) = loc
  location (Con consig) = Ext.location consig

-- |Check that a function or constructor exists and return its signature.
inferFunOrCon :: Location -> Identifier -> Checker FunOrConSig
inferFunOrCon loc name = do
  p <- getProgram
  case lookup name (functions p) of
    Just funsig -> return (Fun (name, funsig))
    Nothing -> case find match (constructors p) of
      Just consig -> return (Con consig)
      Nothing -> failAt loc "Missing Definition"
  where
    match sig = Ext.name sig == name

-- |Check that a function or constructor (for a given codomain)
-- exists and return its signature.
checkFunOrCon :: Location -> Identifier -> Int.Type -> Checker FunOrConSig
checkFunOrCon loc name t = do
  funOrCon <- inferFunOrCon loc name
  when (Ext.returnType funOrCon /= t) $ do
    failAt loc "Missing Definition"
  return funOrCon

-- |Types of the variables bound in a pattern.
type Context = [(Int.Identifier, Int.Type)]

-- |Only keep the first type for each identifier.
nubContext :: Context -> Context
nubContext c = nubBy f c
  where
    f (a, _) (b, _) = a == b

-- |Extract variable types from a typed pattern.
patContext :: Int.Pat -> Context
patContext (Int.VarPat t n) = [(n, t)]
patContext (Int.ConPat _ _ args) = concat $ map patContext args

-- |Extract variable types from a typed copattern.
copContext :: Int.Cop -> Context
copContext (Int.AppCop _ _ args) = concat $ map patContext args
copContext (Int.DesCop _ _ args inner) = concat [copContext inner, concat $ map patContext args]

-- |A zipWithM that requires identical lengths.
zipStrict :: Location -> Location -> (a -> b -> Checker c) -> [a] -> [b] -> Checker [c]
zipStrict loc _loc' f a b
  | length a == length b = zipWithM f a b
  | otherwise            = failAt loc "Length Mismatch"

-- |Typecheck a pattern
checkPat :: Ext.Pat -> Int.Type -> Checker Int.Pat
checkPat (Ext.VarPat _loc name) t = return (Int.VarPat t name)
checkPat (Ext.ConPat loc name args) t = do
  Ext.ConSig loc' _ _ argTypes <- checkCon loc name t
  zipStrict loc loc' checkPat args argTypes >>= return . Int.ConPat t name

-- |Typecheck a copattern. Takes hole type.
inferCop :: Ext.Cop -> FunSig -> Checker Int.Cop
inferCop (Ext.AppCop loc name args) (name', (loc', argTypes, returnType))
    | name == name' = do
        targs <- zipStrict loc loc' checkPat args argTypes
        return $ Int.AppCop returnType name targs
    | otherwise     = failAt loc $
        "Definition Mismatch: " ++ name ++ " used in copattern for " ++ name'
inferCop (Ext.DesCop loc name args inner) s = do
  Ext.DesSig loc' returnType _ argTypes innerType <- inferDes loc name
  tinner <- checkCop inner s innerType
  targs <- zipStrict loc loc' checkPat args argTypes
  return $ Int.DesCop returnType name targs tinner

-- |Typecheck a copattern. Takes hole type and expected return type.
checkCop :: Ext.Cop -> FunSig -> Int.Type -> Checker Int.Cop
checkCop (Ext.AppCop loc name args) (name', (loc', argTypes, returnType)) t
    | name == name', returnType == t = do
        targs <- zipStrict loc loc' checkPat args argTypes
        return $ Int.AppCop returnType name targs
    | otherwise     = failAt loc "Definition Mismatch"
checkCop (Ext.DesCop loc name args inner) s t = do
  Ext.DesSig loc' returnType _ argTypes innerType <- checkDes loc name t
  tinner <- checkCop inner s t
  targs <- zipStrict loc loc' checkPat args argTypes
  return $ Int.DesCop returnType name targs tinner

-- |Typecheck a term.
checkExp :: Context -> Ext.Exp -> Int.Type -> Checker Int.Exp
checkExp c (Ext.VarExp loc n) t = case lookup n c of
    Just t' | t' == t   -> return (Int.VarExp t n)
            | otherwise -> failAt loc $ "Type Mismatch: " ++ n ++
                " expected to be " ++ typeName t ++ " but is actually " ++ typeName t'
    Nothing             -> failAt loc $ "Unbound Variable: " ++ n
checkExp c (Ext.AppExp loc name args) t = do
  funOrCon <- checkFunOrCon loc name t
  targs <- zipStrict loc (Ext.location funOrCon) (checkExp c) args (Ext.argumentTypes funOrCon)
  return $ case funOrCon of
    Fun _ -> Int.AppExp t name targs
    Con _ -> Int.ConExp t name targs
checkExp c (Ext.DesExp loc name args inner) t = do
  Ext.DesSig loc' _ _ argTypes innerType <- checkDes loc name t
  tinner <- checkExp c inner innerType
  targs <- zipStrict loc loc' (checkExp c) args argTypes
  return $ Int.DesExp t name targs tinner

-- |Infer the type of a term.
inferExp :: Context -> Ext.Exp -> Checker Int.Exp
inferExp context (Ext.VarExp loc name) = case lookup name context of
    Nothing  -> failAt loc "Unbound Variable"
    Just typ -> return (Int.VarExp typ name)
inferExp c (Ext.AppExp loc name args) = do
  funOrCon <- inferFunOrCon loc name
  targs <- zipStrict loc (Ext.location funOrCon) (checkExp c) args (Ext.argumentTypes funOrCon)
  return $ case funOrCon of
    Fun _ -> Int.AppExp (Ext.returnType funOrCon) name targs
    Con _ -> Int.ConExp (Ext.returnType funOrCon) name targs
inferExp c (Ext.DesExp loc name args inner) = do
  Ext.DesSig loc' returnType _ argTypes innerType <- inferDes loc name
  tinner <- checkExp c inner innerType
  targs <- zipStrict loc loc' (checkExp c) args argTypes
  return $ Int.DesExp returnType name targs tinner

-- |Identify a type to the user.
typeName :: Int.Type -> Identifier
typeName (Int.Type n) = n

-- |Typecheck a rule against the function's signature.
checkRule :: FunSig -> Ext.Rule -> Checker Int.Rule
checkRule s (Ext.Rule loc left right) = do
    tleft <- inferCop left s
    let c = copContext tleft
    tright <- checkExp c right (Ext.returnType tleft)
    let d = nubContext c
    if length c == length d then
        return (tleft, tright)
    else
        failAt loc "Shadowed Variable"

-- |Fold to collect definitions.
preCheckDef :: Ext.Def -> Checker ()
preCheckDef (Ext.DatDef loc name cons') = do
    prog@(Program names cons _ _ _) <- getProgram
    when (name `elem` names) $ do
      failAt loc "Shadowed Definition"
    when (any mismatch cons') $ do
      failAt loc "Definition Mismatch"
    setProgram prog {
          typeNames = (name:names)
        , constructors = cons ++ cons'
        }
  where
    mismatch (Ext.ConSig _loc' returnType _ _) = returnType /= name
preCheckDef (Ext.CodDef loc name des') = do
    prog@(Program names _ des _ _) <- getProgram
    when (name `elem` names) $ do
      failAt loc "Shadowed Definition"
    when (any mismatch des') $ do
      failAt loc "Definition Mismatch"
    setProgram prog {
          typeNames = (name:names)
        , destructors = des ++ des'
        }
  where
    mismatch (Ext.DesSig _loc' _ _ _ innerType) = innerType /= name
preCheckDef (Ext.FunDef loc name argTypes returnType _) = do
        prog@(Program _ _ _ funs rulz) <- getProgram
        when (any clash rulz) $ do
          failAt loc "Shadowed Definition"
        let sig = (name, (loc, argTypes, returnType))
        setProgram prog {
              functions = (sig:funs)
            }
  where
    clash (name', _) = name' == name

-- |Fold to typecheck definitions.
postCheckDef :: Ext.Def -> Checker ()
postCheckDef (Ext.DatDef loc name cons') = do
    Program names _ _ _ _ <- getProgram
    when (any (missing names) cons') $ do
      failAt loc "Missing Definition"
  where
    missing names (Ext.ConSig _loc' _ _ args)        = (nub args) \\ (name:names) /= []
postCheckDef (Ext.CodDef loc name des') = do
    Program names _ _ _ _ <- getProgram
    when (any (missing names) des') $ do
      failAt loc $
        "Missing Definition: " ++ typeName name ++
        " has a destructor with an unknown argument type"
  where
    missing names (Ext.DesSig _loc' _ _ args _)       = (nub args) \\ (name:names) /= []
postCheckDef (Ext.FunDef loc name argTypes returnType rs)
    = do
        prog@(Program _ _ _ _ rulz) <- getProgram
        let sig = (name, (loc, argTypes, returnType))
        trs <- mapM (checkRule sig) rs
        setProgram prog {
              rules = ((name, trs):rulz)
            }

-- |Turn parser output into interpreter input.
typecheck :: [Ext.Def] -> Checker Program
typecheck defs = do
    traverse_ preCheckDef defs
    traverse_ postCheckDef defs
    getProgram
