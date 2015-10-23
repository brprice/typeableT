-- | Stability: experimental
--
-- This module mocks up a Static Pointer Table (SPT) of common
-- functions, until GHC gets support for the static keyword and can
-- generate one itself

{-# LANGUAGE Safe #-}
{-# LANGUAGE GADTs , RankNTypes , TypeFamilies #-}

module Data.StaticPtr.SPT (initStaticMonoTable
                          ,typeRepDict
                          ,typeRepTypeable
                          ,Tag(typeableConstraint)
                          ,initStaticPolyTable
                          ) where

import Data.ByteString.Lazy(ByteString)
import Data.Map(empty)

import Data.TypeableT(TypeRep(),unsafeTemporaryMkTypeRep,Typeable(typeRep))

import Data.StaticPtr.Internals

instance Typeable ByteString where
  typeRep = unsafeTemporaryMkTypeRep ("bytes_6elQVSg5cWdFrvRnfxTUrH","Data.ByteString.Lazy.Internal","ByteString")

-- MOCK UP of static keyword's effects and a STP
--static :: Typeable a => a -> StaticPtr a

typeRepDict :: TypeRep Dict
typeRepDict = unsafeTemporaryMkTypeRep ("staticptr","Data.StaticPtr.Internals","Dict")
instance Typeable Dict where
  typeRep = typeRepDict

typeRepTypeable :: TypeRep Typeable
typeRepTypeable = unsafeTemporaryMkTypeRep ("typerepT","Data.TypeableT.Inst","Typeable")
instance Typeable Typeable where
  typeRep = typeRepTypeable

initStaticMonoTable :: StaticPtrTable
initStaticMonoTable = SPT empty

initStaticPolyTable :: PolyPtrTable
initStaticPolyTable = PPT $ empty
