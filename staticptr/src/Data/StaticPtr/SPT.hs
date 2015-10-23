-- | Stability: experimental
--
-- This module mocks up a Static Pointer Table (SPT) of common
-- functions, until GHC gets support for the static keyword and can
-- generate one itself

{-# LANGUAGE Safe #-}
{-# LANGUAGE GADTs , RankNTypes , TypeFamilies #-}

module Data.StaticPtr.SPT (spt
                          ,staticNot
                          ,staticTrue
                          ,staticBsNot
                          ,staticDecBool
                          ,staticDecInt
                          ,staticSucc
                          ,staticAdd
                          ,staticPredBS
                          ,typeRepDict
                          ,typeRepTypeable
                          ,typeRepBinary
                          ,staticDBBool
                          ,staticDBInt
                          ,staticDTBool
                          ,staticDTInt
                          ,Tag(typeableConstraint)
                          ,PolyTblEnt(PolyTblEnt)
                          ,ppt
                          ,sExtractDecode
                          ,sMaybeDB
                          ,sMaybeDT
                          ,sListDB
                          ,sListDT
                          ,sReverse
                          ) where

import Data.ByteString.Lazy(ByteString)
import Data.Binary(Binary(),decode)

import Data.TypeableT(TypeRep(),unsafeTemporaryMkTypeRep,Typeable(typeRep))
import Data.DynamicT(SDynamic(),toSDyn)

import Data.StaticPtr.Internals

instance Typeable ByteString where
  typeRep = unsafeTemporaryMkTypeRep ("bytes_6elQVSg5cWdFrvRnfxTUrH","Data.ByteString.Lazy.Internal","ByteString")

-- MOCK UP of static keyword's effects and a STP
--static :: Typeable a => a -> StaticPtr a

staticNot :: StaticPtr (Bool -> Bool)
staticNot = StaticPtr 0 not
staticTrue :: StaticPtr Bool
staticTrue = StaticPtr 1 True
staticBsNot :: StaticPtr (ByteString -> Bool)
staticBsNot = StaticPtr 2 $ not.decode
staticDecBool :: StaticPtr (ByteString -> Bool)
staticDecBool = StaticPtr 3 decode
staticDecInt :: StaticPtr (ByteString -> Int)
staticDecInt = StaticPtr 4 decode
staticSucc :: StaticPtr (Int -> Int)
staticSucc = StaticPtr 5 succ
staticAdd :: StaticPtr (Int -> Int -> Int)
staticAdd = StaticPtr 6 (+)
staticPredBS :: StaticPtr (ByteString -> Int)
staticPredBS = StaticPtr 7 $ pred.decode

typeRepDict :: TypeRep Dict
typeRepDict = unsafeTemporaryMkTypeRep ("staticptr","Data.StaticPtr.Internals","Dict")
instance Typeable Dict where
  typeRep = typeRepDict

typeRepTypeable :: TypeRep Typeable
typeRepTypeable = unsafeTemporaryMkTypeRep ("typerepT","Data.TypeableT.Inst","Typeable")
instance Typeable Typeable where
  typeRep = typeRepTypeable

typeRepBinary :: TypeRep Binary
typeRepBinary = unsafeTemporaryMkTypeRep ("binar_EKE3c9Lmxb3DQpU0fPtru6","Data.Binary.Class","Get")
instance Typeable Binary where
  typeRep = typeRepBinary

staticDTBool :: StaticPtr (Dict (Typeable Bool))
staticDTBool = StaticPtr 8 Dict
staticDTInt :: StaticPtr (Dict (Typeable Int))
staticDTInt = StaticPtr 9 Dict
staticDBBool :: StaticPtr (Dict (Binary Bool))
staticDBBool = StaticPtr 10 Dict
staticDBInt :: StaticPtr (Dict (Binary Int))
staticDBInt = StaticPtr 11 Dict


spt :: [SDynamic StaticPtr]
spt = [toSDyn staticNot
      ,toSDyn staticTrue
      ,toSDyn staticBsNot
      ,toSDyn staticDecBool
      ,toSDyn staticDecInt
      ,toSDyn staticSucc
      ,toSDyn staticAdd
      ,toSDyn staticPredBS
      ,toSDyn staticDTBool
      ,toSDyn staticDTInt
      ,toSDyn staticDBBool
      ,toSDyn staticDBInt
      ]

data ExtractDecodeTag = ExtractDecodeTag deriving Show
instance Tag ExtractDecodeTag where
  type PolyTag ExtractDecodeTag a = Dict (Binary a) -> ByteString -> a
  typeableConstraint _ Dict = Dict

data MaybeDBTag = MaybeDBTag deriving Show
instance Tag MaybeDBTag where
  type PolyTag MaybeDBTag a = Dict (Binary a) -> Dict (Binary (Maybe a))
  typeableConstraint _ Dict = Dict

data MaybeDTTag = MaybeDTTag deriving Show
instance Tag MaybeDTTag where
  type PolyTag MaybeDTTag a = Dict (Typeable (Maybe a))
  typeableConstraint _ Dict = Dict

data ListDBTag = ListDBTag deriving Show
instance Tag ListDBTag where
  type PolyTag ListDBTag a = Dict (Binary a) -> Dict (Binary [a])
  typeableConstraint _ Dict = Dict

data ListDTTag = ListDTTag deriving Show
instance Tag ListDTTag where
  type PolyTag ListDTTag a = Dict (Typeable [a])
  typeableConstraint _ Dict = Dict

data RevTag = RevTag deriving Show
instance Tag RevTag where
  type PolyTag RevTag a = [a] -> [a]
  typeableConstraint _ Dict = Dict

-- There is a fair amount of duplication in building the PPT,
-- as extracting the relevent parts is annoying,
-- and this should all be automated by GHC

sExtractDecode :: Static (Dict (Typeable (a :: *))) -> Static (PolyTag ExtractDecodeTag a)
sExtractDecode ta = PolyPtr 0 ExtractDecodeTag ta (\_ Dict -> decode)

sMaybeDB :: Static (Dict (Typeable (a :: *))) -> Static (PolyTag MaybeDBTag a)
sMaybeDB ta = PolyPtr 1 MaybeDBTag ta (\_ Dict  -- :: Dict (Binary b))   -- Note the confusing types here!
                                       -> Dict) -- :: Dict (Binary (Maybe b)))

sMaybeDT :: Static (Dict (Typeable (a :: *))) -> Static (PolyTag MaybeDTTag a)
sMaybeDT ta = PolyPtr 2 MaybeDTTag ta (\Dict    -- :: Dict (Typeable b))
                                       -> Dict) -- :: Dict (Typeable (Maybe b)))

sListDB :: Static (Dict (Typeable (a :: *))) -> Static (PolyTag ListDBTag a)
sListDB ta = PolyPtr 3 ListDBTag ta (\_ Dict  -- :: Dict (Binary b))
                                     -> Dict) -- :: Dict (Binary [b]))

sListDT :: Static (Dict (Typeable (a :: *))) -> Static (PolyTag ListDTTag a)
sListDT ta = PolyPtr 4 ListDTTag ta (\Dict    -- :: Dict (Typeable b))
                                     -> Dict) -- :: Dict (Typeable [b]))


sReverse :: Static (Dict (Typeable (a :: *))) -> Static (PolyTag RevTag a)
sReverse ta = PolyPtr 5 RevTag ta (\_ xs -> reverse xs)

data PolyTblEnt where
  PolyTblEnt :: Tag t => t -> (forall (a :: *) . Dict (Typeable a) -> PolyTag t a) -> PolyTblEnt

eExtractDecode :: PolyTblEnt
eExtractDecode = PolyTblEnt ExtractDecodeTag (\_ Dict -> decode)

eMaybeDB :: PolyTblEnt
eMaybeDB = PolyTblEnt MaybeDBTag  (\_ Dict  -- :: Dict (Binary b))   -- Note the confusing types here!
                                   -> Dict) -- :: Dict (Binary (Maybe b)))

eMaybeDT :: PolyTblEnt
eMaybeDT = PolyTblEnt MaybeDTTag (\Dict    -- :: Dict (Typeable b))
                                  -> Dict) -- :: Dict (Typeable (Maybe b)))

eListDB :: PolyTblEnt
eListDB = PolyTblEnt ListDBTag (\_ Dict  -- :: Dict (Binary b))
                                -> Dict) -- :: Dict (Binary [b]))

eListDT :: PolyTblEnt
eListDT = PolyTblEnt ListDTTag (\Dict    -- :: Dict (Typeable b))
                                -> Dict) -- :: Dict (Typeable [b]))

eReverse :: PolyTblEnt
eReverse = PolyTblEnt RevTag (\_ xs -> reverse xs)

ppt :: [PolyTblEnt]
ppt = [eExtractDecode
      ,eMaybeDB
      ,eMaybeDT
      ,eListDB
      ,eListDT
      ,eReverse
      ]
