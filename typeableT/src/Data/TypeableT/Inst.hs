-- | Stability: experimental
-- Temporary hand-crafted 'TypeRep' values and 'Typeable' instances for common data types, until GHC gets in-built support

{-# LANGUAGE Safe #-}
{-# LANGUAGE PolyKinds , FlexibleInstances , ScopedTypeVariables #-}

module Data.TypeableT.Inst where

import Data.TypeableT.Internals
import Data.TyConT.Inst

-- | Class of those types which have a runtime reflection.
-- This is kind-polymorphic (@a :: k@).
class Typeable (a :: k) where
  typeRep :: TypeRep a

{- COMPILER MAGIC SHOULD GO HERE & replace the whole module -}
-- GHC should attain magic built-in support for Typeable instances
-- but the effect will be similar to declarations like these:

instance (Typeable c, Typeable a) => Typeable (c a) where
  typeRep = TRApp (typeRep :: TypeRep c) (typeRep :: TypeRep a)

typeRepBool :: TypeRep Bool
typeRepBool = TRCon tyConBool

typeRepInt :: TypeRep Int
typeRepInt = TRCon tyConInt

typeRepMaybe :: TypeRep Maybe
typeRepMaybe = TRCon tyConMaybe

typeRepVoid :: TypeRep ()
typeRepVoid = TRCon tyConVoid

typeRepArr :: TypeRep (->)
typeRepArr = TRCon tyConArr

typeRepList :: TypeRep []
typeRepList = TRCon tyConList

typeRepTypeRep :: TypeRep TypeRep
typeRepTypeRep = TRCon tyConTypeRep

instance Typeable Bool where
  typeRep = typeRepBool
instance Typeable Int where
  typeRep = typeRepInt
instance Typeable Maybe where
  typeRep = typeRepMaybe
instance Typeable () where
  typeRep = typeRepVoid
instance Typeable (->) where
  typeRep = typeRepArr
instance Typeable [] where
  typeRep = typeRepList
instance Typeable TypeRep where
  typeRep = typeRepTypeRep
