-- | Stability: experimental
--
-- This provides the API of Typeable from GHC 7.10, base package, to ease the transition
-- Warning: we do not yet have efficient equality tests and fingerprint calculation.
-- Warning: the show instance for @TypeRep@ differs from that of @Data.Typeable.TypeRep@ from base.

{-# LANGUAGE Trustworthy #-} -- because of Data.Type.Equality
{-# LANGUAGE ConstraintKinds , ExplicitForAll , PolyKinds , TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-} -- don't warn when compiling this module!

module Data.TypeableT.Typeable710 {-# DEPRECATED "Use Data.TypeableT instead (new API)" #-}
         (-- * The Typeable class
          Typeable
         ,typeRep

          -- * Propositional equality
         ,(:~:)(Refl)

          -- * For further backwards compatibility
         ,typeOf, typeOf1, typeOf2, typeOf3, typeOf4, typeOf5, typeOf6, typeOf7
         ,Typeable1, Typeable2, Typeable3, Typeable4, Typeable5, Typeable6, Typeable7

          -- * Type-safe cast
         ,cast
         ,eqT
         ,gcast                  -- a generalisation of cast

          -- * Generalized casts for higher-order kinds
         ,gcast1                 -- :: ... => c (t a) -> Maybe (c (t' a))
         ,gcast2                 -- :: ... => c (t a b) -> Maybe (c (t' a b))

           -- * A canonical proxy type
         ,Proxy (..)

          -- * Type representations
         ,TypeRep        -- abstract, instance of: Eq, Show, Typeable
         ,typeRepFingerprint
         ,rnfTypeRep
         ,showsTypeRep

         ,TyCon          -- abstract, instance of: Eq, Show, Typeable
         ,tyConFingerprint
         ,tyConString
         ,tyConPackage
         ,tyConModule
         ,tyConName
         ,rnfTyCon

          -- * Construction of type representations
          -- mkTyCon,        -- :: String  -> TyCon
         ,mkTyCon3       -- :: String  -> String -> String -> TyCon
         ,mkTyConApp     -- :: TyCon   -> [TypeRep] -> TypeRep
         ,mkAppTy        -- :: TypeRep -> TypeRep   -> TypeRep
         ,mkFunTy        -- :: TypeRep -> TypeRep   -> TypeRep

          -- * Observation of type representations
         ,splitTyConApp  -- :: TypeRep -> (TyCon, [TypeRep])
         ,funResultTy    -- :: TypeRep -> TypeRep   -> Maybe TypeRep
         ,typeRepTyCon   -- :: TypeRep -> TyCon
         ,typeRepArgs    -- :: TypeRep -> [TypeRep]
         ) where

import Data.Proxy(Proxy(..))
import Data.Type.Equality((:~:)(Refl))
import GHC.Fingerprint.Type(Fingerprint)

import qualified Data.TypeableT as TT

type Typeable a = TT.Typeable710 a

-- | Takes a value of type @a@ and returns a concrete representation
-- of that type.
typeRep :: forall proxy a. Typeable a => proxy a -> TypeRep
typeRep = TT.typeRep710

typeOf :: forall a. Typeable a => a -> TypeRep
typeOf = TT.typeOf710
typeOf1 :: forall t a. Typeable t => t a -> TypeRep
typeOf1 = TT.typeOf1710
typeOf2 :: forall t a b. Typeable t => t a b -> TypeRep
typeOf2 = TT.typeOf2710
typeOf3 :: forall t a b c. Typeable t => t a b c -> TypeRep
typeOf3 = TT.typeOf3710
typeOf4 :: forall t a b c d. Typeable t => t a b c d -> TypeRep
typeOf4 = TT.typeOf4710
typeOf5 :: forall t a b c d e. Typeable t => t a b c d e -> TypeRep
typeOf5 = TT.typeOf5710
typeOf6 :: forall t a b c d e f. Typeable t => t a b c d e f -> TypeRep
typeOf6 = TT.typeOf6710
typeOf7 :: forall t a b c d e f g. Typeable t => t a b c d e f g -> TypeRep
typeOf7 = TT.typeOf7710

type Typeable1 a = Typeable a
type Typeable2 a = Typeable a
type Typeable3 a = Typeable a
type Typeable4 a = Typeable a
type Typeable5 a = Typeable a
type Typeable6 a = Typeable a
type Typeable7 a = Typeable a

-- | The type-safe cast operation
cast :: forall a b. (Typeable a, Typeable b) => a -> Maybe b
cast = TT.cast710

-- | Extract a witness of equality of two types
eqT :: forall a b. (Typeable a, Typeable b) => Maybe (a :~: b)
eqT = TT.eqT710

-- | A flexible variation parameterised in a type constructor
gcast :: forall a b c. (Typeable a, Typeable b) => c a -> Maybe (c b)
gcast = TT.gcast710

-- | Cast over @k1 -> k2@
gcast1 :: forall c t t' a. (Typeable t, Typeable t') => c (t a) -> Maybe (c (t' a))
gcast1 = TT.gcast1710

-- | Cast over @k1 -> k2 -> k3@
gcast2 :: forall c t t' a b. (Typeable t, Typeable t') => c (t a b) -> Maybe (c (t' a b))
gcast2 = TT.gcast2710

-- | A concrete representation of a (monomorphic) type.  'TypeRep'
-- supports reasonably efficient equality.
-- TODO: NB: verbatim doc from @base@ - we don't have efficient equality yet!
type TypeRep = TT.TypeRep710

-- | Observe the 'Fingerprint' of a type representation
typeRepFingerprint :: TypeRep -> Fingerprint
typeRepFingerprint = TT.typeRepFingerprint710

-- | Helper to fully evaluate 'TypeRep' for use as @NFData(rnf)@ implementation
rnfTypeRep :: TypeRep -> ()
rnfTypeRep = TT.rnfTypeRep710

showsTypeRep :: TypeRep -> ShowS
showsTypeRep = TT.showsTypeRep710

-- | An abstract representation of a type constructor.  'TyCon' objects can
-- be built using 'mkTyCon3'.
type TyCon = TT.TyCon710

tyConFingerprint :: TyCon -> Fingerprint
tyConFingerprint = TT.tyConFingerprint710

-- | Observe string encoding of a type representation
tyConString :: TyCon -> String
tyConString = TT.tyConString710

tyConPackage :: TyCon -> String
tyConPackage = TT.tyConPackage710

tyConModule :: TyCon -> String
tyConModule = TT.tyConModule710

tyConName :: TyCon -> String
tyConName = TT.tyConName710

-- | Helper to fully evaluate 'TyCon' for use as @NFData(rnf)@ implementation
rnfTyCon :: TyCon -> ()
rnfTyCon = TT.rnfTyCon710

-- | Builds a 'TyCon' object representing a type constructor.  An
-- implementation should ensure that the following holds:
--
-- >  A==A' ^ B==B' ^ C==C' ==> mkTyCon A B C == mkTyCon A' B' C'
--

--
mkTyCon3 :: String    -- ^ package name
         -> String    -- ^ module name
         -> String    -- ^ the name of the type constructor
         -> TyCon  -- ^ A unique 'TyCon' object
mkTyCon3 = TT.mkTyCon3710

-- | Applies a monomorphic type constructor to a sequence of types
mkTyConApp :: TyCon -> [TypeRep] -> TypeRep
mkTyConApp = TT.mkTyConApp710

-- | Adds a TypeRep argument to a TypeRep.
mkAppTy :: TypeRep -> TypeRep -> TypeRep
mkAppTy = TT.mkAppTy710

-- | A special case of 'mkTyConApp', which applies the function
-- type constructor to a pair of types.
mkFunTy :: TypeRep -> TypeRep -> TypeRep
mkFunTy = TT.mkFunTy710

-- | Splits a type constructor application.
-- Note that if the type construcotr is polymorphic, this will
-- not return the kinds that were used.
splitTyConApp :: TypeRep -> (TyCon, [TypeRep])
splitTyConApp = TT.splitTyConApp710

-- | Applies a type to a function type.  Returns: @'Just' u@ if the
-- first argument represents a function of type @t -> u@ and the
-- second argument represents a function of type @t@.  Otherwise,
-- returns 'Nothing'.
funResultTy :: TypeRep -> TypeRep -> Maybe TypeRep
funResultTy = TT.funResultTy710

-- | Observe the type constructor of a type representation
typeRepTyCon :: TypeRep -> TyCon
typeRepTyCon = TT.typeRepTyCon710

-- | Observe the argument types of a type representation
typeRepArgs :: TypeRep -> [TypeRep]
typeRepArgs = TT.typeRepArgs710
