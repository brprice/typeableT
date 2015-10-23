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

typeRepChar :: TypeRep Char
typeRepChar = TRCon tyConChar

typeRepTuple2 :: TypeRep (,)
typeRepTuple2 = TRCon tyConTuple2

typeRepTuple3 :: TypeRep (,,)
typeRepTuple3 = TRCon tyConTuple3

typeRepTuple4 :: TypeRep (,,,)
typeRepTuple4 = TRCon tyConTuple4

typeRepTuple5 :: TypeRep (,,,,)
typeRepTuple5 = TRCon tyConTuple5

typeRepMaybe :: TypeRep Maybe
typeRepMaybe = TRCon tyConMaybe

typeRepVoid :: TypeRep ()
typeRepVoid = TRCon tyConVoid

typeRepArr :: TypeRep (->)
typeRepArr = TRCon tyConArr

typeRepList :: TypeRep []
typeRepList = TRCon tyConList

typeRepEither :: TypeRep Either
typeRepEither = TRCon tyConEither

typeRepTypeRep :: TypeRep TypeRep
typeRepTypeRep = TRCon tyConTypeRep

instance Typeable Bool where
  typeRep = typeRepBool
instance Typeable Int where
  typeRep = typeRepInt
instance Typeable Char where
  typeRep = typeRepChar
instance Typeable (,) where
  typeRep = typeRepTuple2
instance Typeable (,,) where
  typeRep = typeRepTuple3
instance Typeable (,,,) where
  typeRep = typeRepTuple4
instance Typeable (,,,,) where
  typeRep = typeRepTuple5
instance Typeable Maybe where
  typeRep = typeRepMaybe
instance Typeable () where
  typeRep = typeRepVoid
instance Typeable (->) where
  typeRep = typeRepArr
instance Typeable [] where
  typeRep = typeRepList
instance Typeable Either where
  typeRep = typeRepEither
instance Typeable TypeRep where
  typeRep = typeRepTypeRep
