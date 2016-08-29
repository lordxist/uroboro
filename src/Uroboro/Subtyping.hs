{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}


{-|
Description : Subtyping relation

Determine whether two types are in the subtyping relation
-}
module Uroboro.Subtyping
    (extensionRelation, supertypeRelation, SubtypeVariant (NoSubtyp, ExtSums, ExtAlgDat), readWithDefault) where

import Data.Maybe (fromJust)
import Data.Monoid (Monoid, mempty, mappend)

import Control.Arrow ((***))
import Control.Monad.Reader

import Uroboro.Tree.External

-- |Subtyping relation monad.
type Subtyping = ReaderT [Def] (Reader (Def, Def)) Bool

instance Monoid Subtyping where
  mempty = lift $ reader $ const True
  m1 `mappend` m2 = fmap (||) m1 <*> m2

-- |Find the definition of the given type if exists, otherwise return Nothing.
findDef :: [Def] -> Type -> Maybe Def
findDef defs t = do
    let tdefs = [ x | x <- defs, (name t) == (name x) ]
    if (length tdefs) == 1
      then Just $ tdefs !! 0
      else Nothing

-- |Find the definition of the given type, assuming it exists.
findDef_ :: [Def] -> Type -> Def
findDef_ defs t = fromJust $ findDef defs t

-- |Sub-relation encompassing only defs for the same type.
sameDef :: Subtyping
sameDef = do
    (def1, def2) <- lift ask
    return $ (name def1) == (name def2)

-- |Sub-relation encompassing only defs for different types.
irreflexiveExt :: Subtyping
irreflexiveExt = do 
    (def1, def2) <- lift ask
    defs <- ask
    let maybePtype1 = parent def1
    case maybePtype1 of
      Just ptype1 -> do
        let maybePdef1 = findDef defs ptype1
        case maybePdef1 of
          Just pdef1 -> mapReaderT (local (\(def1, def2) -> (pdef1, def2))) extension
          Nothing -> return False
      Nothing -> return False
  where
    parent (DatDef _ _ p _) = p
    parent (CodDef _ _ p _) = p

-- |Complete subtyping relation.
extension :: Subtyping
extension = sameDef `mappend` irreflexiveExt


extensionRelation :: [Def] -> Type -> Type -> Bool
extensionRelation defs t1 t2 = runReader (runReaderT extension defs) (findDef_ defs t1, findDef_ defs t2)

-- | Subtype variant: no subtyping, extensible sums-, or extensible algebraic datatypes-like subtyping.
data SubtypeVariant = NoSubtyp | ExtSums | ExtAlgDat deriving (Show)

-- | Default subtype variant: no subtyping
defaultSubtypeVariant :: SubtypeVariant
defaultSubtypeVariant = NoSubtyp

-- | Converts a string to a SubtypeVariant if it is literally the same as one of its constructor names, otherwise returns NoSubtyp.
readWithDefault :: String -> SubtypeVariant
readWithDefault s
  | s == show ExtSums   = ExtSums
  | s == show ExtAlgDat = ExtAlgDat
  | otherwise           = NoSubtyp


-- | Supertype relation.
supertypeRelation :: SubtypeVariant -> [Def] -> Type -> Type -> Bool
supertypeRelation NoSubtyp _     = (==)
supertypeRelation ExtSums defs   = extensionRelation defs
supertypeRelation ExtAlgDat defs = flip $ extensionRelation defs
