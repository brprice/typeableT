-- |
-- Stability : experimental
--
-- This module exposes internals and is for the sole use of the
-- "Data.TypeableT" module, which re-exports hiding stuff and
-- "Data.TypeableT.Inst", which provides the
-- 'Data.TypeableT.Typeable' class and instances, and
-- 'Data.TypeableT.typeRepBool' et. al.; and "Data.TyConT.Inst", for
-- 'Data.TyConT.tyConTypeRep'.  This will get nicer once GHC is
-- magic enough, as @TyConT.Inst@ and @Typeable.Inst@ will disappear
-- and this module can be folded into "Data.TypeableT".

{-# LANGUAGE Safe #-}
{-# LANGUAGE GADTs , PolyKinds #-}


module Data.TypeableT.Internals (TypeRep(TRCon,TRApp)) where

import Data.TyConT

-- Haddock note: when this doc is put into the public-facing modules, it will be abstract.
-- | Abstract type which is a runtime reflection of types.
-- This is kind-polymorphic (@a :: k@).
data TypeRep (a :: k) :: * where
  TRCon :: TyCon a -> TypeRep a
  TRApp :: TypeRep a -> TypeRep b -> TypeRep (a b)

instance Show (TypeRep a) where
  showsPrec _ (TRCon c) = shows c
  showsPrec d (TRApp c a) = showParen (d>p) $ showsPrec (p+1) c . showString " " . showsPrec (p+1) a
    where p = 10
