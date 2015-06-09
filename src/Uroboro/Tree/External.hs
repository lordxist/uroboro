{-|
Description : Parse tree.

Representation of Uroboro programs as abstract syntax tree. This
is the "external" program representation produced by the parser.
-}

module Uroboro.Tree.External
       ( -- * Common parts
         -- $common
         Identifier
       , Type (Type)
         -- * Parse tree
       , Exp (VarExp, AppExp, DesExp)
       , Pat (VarPat, ConPat)
       , Cop (AppCop, DesCop)
       , ConSig (ConSig)
       , DesSig (DesSig)
       , Rule (Rule)
       , Def (DatDef, CodDef, FunDef)
         -- * Overloaded Acessors
       , HasReturnType (returnType)
       , HasArgumentTypes (argumentTypes)
       , HasLocation (location)
       ) where

import Uroboro.Error (Location)
import Uroboro.Tree.Common

-- $common
-- Reexported from "Uroboro.Tree.Common".

-- |Expression (Term).
data Exp
    -- |Variable.
    = VarExp Location Identifier
    -- |Constructor or function application.
    | AppExp Location Identifier [Exp]
    -- |Destructor application (Selection).
    | DesExp Location Identifier [Exp] Exp deriving (Show)

-- |Pattern.
data Pat
    -- |Variable pattern.
    = VarPat Location Identifier
    -- |Constructor pattern.
    | ConPat Location Identifier [Pat] deriving (Show)

-- |Copattern.
data Cop
    -- |Hole pattern.
    = AppCop Location Identifier [Pat]
    -- |Destructor pattern.
    | DesCop Location Identifier [Pat] Cop deriving (Show)

-- |Constructor definition.
data ConSig = ConSig Location Type Identifier [Type] deriving (Show)

-- |Destructor definition.
-- Return type first, type to destruct last.
data DesSig = DesSig Location Type Identifier [Type] Type deriving (Show)

-- |Part of a function definition.
data Rule = Rule Location Cop Exp deriving (Show)

-- |Definition.
data Def
    -- |Data type.
    = DatDef Location Type [ConSig]
    -- |Codata type.
    | CodDef Location Type [DesSig]
    -- |Function.
    | FunDef Location Identifier [Type] Type [Rule] deriving (Show)

class HasReturnType t where
  returnType :: t -> Type

instance HasReturnType ConSig where
  returnType (ConSig _ t _ _) = t

instance HasReturnType DesSig where
  returnType (DesSig _ t _ _ _) = t

class HasArgumentTypes t where
  argumentTypes :: t -> [Type]

instance HasArgumentTypes ConSig where
  argumentTypes (ConSig _ _ _ ts) = ts

instance HasArgumentTypes DesSig where
  argumentTypes (DesSig _ _ _ ts _) = ts

class HasLocation t where
  location :: t -> Location

instance HasLocation Exp where
  location (VarExp loc _) = loc
  location (AppExp loc _ _) = loc
  location (DesExp loc _ _ _) = loc

instance HasLocation Pat where
  location (VarPat loc _) = loc
  location (ConPat loc _ _) = loc

instance HasLocation Cop where
  location (AppCop loc _ _) = loc
  location (DesCop loc _ _ _) = loc

instance HasLocation ConSig where
  location (ConSig loc _ _ _) = loc

instance HasLocation DesSig where
  location (DesSig loc _ _ _ _) = loc

instance HasLocation Rule where
  location (Rule loc _ _) = loc

instance HasLocation Def where
  location (DatDef loc _ _) = loc
  location (CodDef loc _ _) = loc
  location (FunDef loc _ _ _ _) = loc
