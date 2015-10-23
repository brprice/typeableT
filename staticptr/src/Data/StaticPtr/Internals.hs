-- | Stability: experimental
--
-- This module exposes internals and is for the sole use of the
-- "Data.StaticPtr" module, which implements more and re-exports hiding stuff and
-- the "Data.StaticPtr.SPT" module which mocks up a SPT (static pointer table).  This will
-- get nicer once GHC is magic enough, as SPT will disppear, and this
-- can be rolled into "Data.StaticPtr"

{-# LANGUAGE Safe #-}
{-# LANGUAGE GADTs , RankNTypes , ConstraintKinds , TypeFamilies #-}

module Data.StaticPtr.Internals(StaticName,StaticPtr(StaticPtr),StaticPtrTable(SPT),Dict(Dict),PolyTblEnt(PolyTblEnt),PolyPtrTable(PPT),Static(MonoPtr,PolyPtr,StaticApp),Tag(PolyTag,typeableConstraint)) where

import Data.TypeableT(Typeable())
import Data.DynamicT(Dynamic())

import Data.Map(Map())

-- | This is the name of a 'StaticPtr' - this is what gets serialised, and is a key into the SPT
type StaticName = String

-- | A opaque data type representing, intuitively, a static /code/ pointer to a value of type @a@
data StaticPtr a = StaticPtr StaticName a

-- | An opaque lookup table for static pointers
newtype StaticPtrTable = SPT (Map StaticName Dynamic)

-- | This gives a way to get an actual value embodying a constraint dictionary so we can pass it around explicitly
data Dict c where
  Dict :: forall c . c => Dict c

-- We need Dict (Typeable a) as, for Serialisable, we need to do
-- Static(TypeRep a) -> Static(TypeRep (Maybe a)) or so,
-- and this is much nicer with Dicts as, like the Binary part,
-- we can use the instance to create the necessary dictionary.

-- Can't do higher kinded args, as need the typeRep expected by table & the one passed to lookup to have same kind. We enforce this to be *
-- | A class for those types which can be used as a tag in a 'PolyPtr'.
class Show t => Tag t where
  -- | An open type family to represent the "return type" of 'PolyPtr', when instantiated at @a@.
  -- The @t@ gives a family of type families, for different 'PolyPtr's.
  -- Notice that this only really allows for one free type variable,
  -- it would be possible to add @Tag2@ etc. if needed.
  -- Note @t :: *@ and @a :: *@, as @t@'s kind is arbitrary (it is only used as a tag here and in the Poly Pointer Table),
  -- but if @a@ is poly-kinded, then it is hard to control kind-mismatches between the argument and the PPT in 'Data.StaticPtr.lookupPPT'
  -- Note that the following is outlawed, so @a@ cannot be a monad for instance
  --
  -- >>> data HighTag = HighTag deriving Show
  -- >>> instance PolyTag HighTag where
  -- >>>   type PolyTag HighTag a = a Bool
  -- >>>   tagTyCtor _ _ = undefined
  type PolyTag t a :: *
  -- | We require that 'PolyTag t a' is 'Typeable' (whenever @a@ is), and this expresses that constraint.
  typeableConstraint :: t -> Dict (Typeable (a :: *)) -> Dict (Typeable (PolyTag t a))
  -- TODO: this could probably be encoded in the PPTEnt, by
  --     (forall b . Dict (Typeable b) -> (TypeRep (PolyTag t b), PolyTag t b))
  -- but it is quite nice to declare it on tag creation (whilst everything is manual, at least)


data PolyTblEnt where
  PolyTblEnt :: Tag t => t -> (forall (a :: *) . Dict (Typeable a) -> PolyTag t a) -> PolyTblEnt

-- | An opaque lookup table for polymorphic static pointers
newtype PolyPtrTable = PPT (Map StaticName PolyTblEnt)


-- Haddock note: PolyPtr not exported from StaticPtr module, so this remark will be true from the user's viewpoint
-- | 'Static' represents a known-at-compile-time value, either a 'StaticPtr',
-- a /sort-of/ polymorphic value 'PolyPtr',
-- or an application of two 'Statics' ('StaticApp').
-- None of these constructors are exported - please use 'staticMono' and 'staticApp'.
--
-- By /sort-of/, we mean that we can't do true polymorphism, as we can't have a @'Data.TypeableT.TypeRep'(forall a. a)@ (because of impredicativity).
-- What we actually turn out to need however, is just
-- "the polymorphic function @f@ with free type variable instantiated at @a@",
-- where @a@ is only _dynamically_ known, and we need this instantiated @f@ to be serialisable.
-- @a@ is only dynamically known as we may have a remote process which reverses lists say (and leaves other types alone), so it recieves
-- a 'SDynamic Closure' of a list, and needs to apply a static \"polymorphic\" @'reverse'@ to it.
-- We obviously don't statically know the type of elements in the list, but the 'SDynamic' gives
-- us a 'TypeRep' for them!
data Static a where
  MonoPtr :: StaticPtr a -> Static a
  -- In PolyPtr note that a :: * and b :: *
  PolyPtr :: Tag t => StaticName -> t -> Static (Dict (Typeable a)) -> (forall b . Dict (Typeable b) -> PolyTag t b) -> Static (PolyTag t a)
  StaticApp :: Static (a -> b) -> Static a -> Static b

instance Show (StaticPtr a) where
  showsPrec d (StaticPtr n _) = showParen (d>p) $ showString "StaticPtr " . showsPrec (p+1) n
    where p = 10

instance Show (Static a) where
  showsPrec d (MonoPtr mp) = showParen (d>p) $ showString "MonoPtr " . showsPrec (p+1) mp
    where p = 10
  showsPrec d (PolyPtr n tag tr _) = showParen (d>p) $ showString "PolyPtr " . showsPrec (p+1) n . showString " " . showsPrec (p+1) tag . showString " " . showsPrec (p+1) tr
    where p = 10
  showsPrec d (StaticApp f x) = showParen (d>p) $ showString "StaticApp " . showsPrec (p+1) f . showString " " . showsPrec (p+1) x
    where p = 10
