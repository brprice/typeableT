{-
Trusted code - uses unsafe coerce etc

unsafeCoerce in: eqTyConHom
-}
-- |
-- Stability : experimental
--
-- This module contains primative operations on 'TyCon's
-- (i.e. comparison for equality).

{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE PolyKinds , TypeOperators #-}

module Data.TyConT (TyCon()
                   ,Id,tyConId,tyConFingerprint
                   ,unsafeTemporaryMkTyCon
                   ,eqTyCon,eqTyConHom
                   ,rnfTyCon) where

import Data.Type.Equality((:~:)(Refl))
import Unsafe.Coerce(unsafeCoerce)

import safe Data.TyConT.Internals

-- | Escape hatch so one can define new 'TyCon's and 'Data.TypeableT.TypeRep's.
-- It is the user's responsability to ensure that the 'Id' is unique for each 'TyCon'.
-- This will be removed when GHC supports deriving 'Data.TypeableT.Typeable'.
unsafeTemporaryMkTyCon :: Id -> TyCon a
unsafeTemporaryMkTyCon = TyCon

-- | Compare two 'TyCon's for equality, even if differently kinded.
-- This returns a Boolean, which is not hugely useful, as we get no
-- static type information.
eqTyCon :: TyCon (a::k1) -> TyCon (b::k2) -> Bool -- can improve with richards branch (kind-hetrogenous equality)
eqTyCon (TyCon id1) (TyCon id2) = id1 == id2

-- | Compare two 'TyCon's for equality, and return /evidence/.
-- Unfortunately, currently GHC cannot handle kind-hetrogenous
-- equalities, so the 'TyCon's must have the same kind.
eqTyConHom :: TyCon (a::k) -> TyCon (b::k) -> Maybe (a :~: b)
eqTyConHom a b | eqTyCon a b = Just $ unsafeCoerce Refl
               | otherwise = Nothing

-- | Helper to fully evaluate 'TyCon' for use as @NFData(rnf)@ implementation
rnfTyCon :: TyCon a -> ()
rnfTyCon (TyCon (p,m,n)) = go $ p++m++n
  where go [] = ()
        go (x:xs) = x `seq` go xs
