-- | Stability: experimental
-- Support for converting a 'TypeRep' value into a 'Typeable' constraint solution


{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE RankNTypes , ScopedTypeVariables #-}


module Data.TypeableT.WithTypeRep (withTypeRep) where

import Unsafe.Coerce(unsafeCoerce)

import Data.TypeableT

-- Hideous hack to ``convert a TypeRep value into a Typeable dictionary''
-- See #2439:25

-- This seems to work, but I don't really understand what's going on.
-- This depends on the internal representation of a single method class being the same as that of the method
-- This may well be fragile to changes in GHC

newtype Magic a b = Magic (Typeable a => b)

-- | This method enables one to use a 'TypeRep' value to discharge a 'Typeable' constraint
withTypeRep :: forall a b . TypeRep a -> (Typeable a => b) -> b
withTypeRep ta f = a ta
  where m :: Magic a b
        m = Magic f
        a :: TypeRep a -> b
        a = unsafeCoerce m
