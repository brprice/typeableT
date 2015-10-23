-- |
-- Stability : experimental
--
-- This module exposes internals and is for the sole use of the "Data.TyConT"
-- module, which re-exports hiding stuff and "Data.TyConT.Inst", which
-- provides 'tyConTBool' et. al.  This will get nicer once GHC is magic enough as
-- "Data.TyConT.Inst" will disappear and this module can be folded into "Data.TyConT".


{-# LANGUAGE Safe #-}
{-# LANGUAGE PolyKinds #-}

module Data.TyConT.Internals (TyCon(TyCon),Id,tyConId,tyConFingerprint) where

import GHC.Fingerprint(Fingerprint(),fingerprintString)

type Package = String
type Module = String
type Name = String

-- | The internal identity of a 'TyCon'
-- It is a 3-tuple of strings: (package-key, module-name, tycon-name)
type Id = (Package,Module,Name)

-- | Observe the 'Fingerprint' of a type representation
tyConFingerprint :: TyCon a -> Fingerprint
tyConFingerprint = fingerprintString . showId . tyConId
  where showId :: Id -> String
        showId (m,p,n) = m ++ " " ++ p ++ " " ++ n

-- Haddock note: when this doc is put into the public-facing modules, it will be abstract.
-- | The abstract type 'TyCon' tags type constructors in a 'Data.TypeableT.TypeRep' representation.
data TyCon (a::k) = TyCon Id

-- | Extract the internal 'Id' of a 'TyCon'
tyConId :: TyCon a -> Id
tyConId (TyCon ident) = ident

instance Show (TyCon a) where
  show (TyCon (_,_,name)) = name
