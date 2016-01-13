{-|
Description : Common parts of trees.

Reusable parts of the various representations of Uroboro programs
as syntax trees.
-}

module Uroboro.Tree.Common
       ( Identifier
       , HasName (name)
       , findByName
       , Type (Type)
       ) where

import Data.List (find)

-- |This is used for type names, function names, constructor and
-- destructor names, as well as variable names.
type Identifier = String

class HasName t where
  name :: t -> Identifier

-- |Look something up by name.
findByName :: HasName t => Identifier -> [t] -> Maybe t
findByName n = find match where
  match t = name t == n

-- |Represents both positive and negative data types.
newtype Type = Type Identifier deriving (Eq, Show)
