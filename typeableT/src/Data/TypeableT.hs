-- | Stability: experimental
--
-- Type-indexed runtime type representations.
--
-- We provide the deprecated interface with '710' as a suffix.
-- See also 'Data.Typeable.Typeable710'.
--
-- Warning: we do not yet have efficient equality tests and fingerprint calculation.
--
-- Warning: the show instance for @TypeRep710@ differs from that of @Data.Typeable.TypeRep@ from base.
--
-- Note: fingerprints of a @TypeRep@ and a @TypeRep710@ representing the same thing are equal, and also equal to the fingerprint of a @Data.Typeable.TypeRep@ from base.
-- (at least, on this machine, at time of writing, and everywhere once GHC automatically generates TypeReps).

-- RE: efficient equality and fingerprints: we don't cache the fingerprints in the values, and to calculate equality, we walk the tree, generating evidence at leaves and patching together
-- it may be worth doing equality by a comparison on fingerprints, and an unsafeCoerce to generate evidence.

{- Eventually untrusted code, but need a few uses of unsafeCoerce until Richard's branch lands (hopefully) - in eqRRHom, getR1 and getR2 -}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE GADTs , PolyKinds , FlexibleInstances , TypeOperators , ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds , UndecidableInstances #-}  -- for compatibility shim: Typeable1 etc

module Data.TypeableT (TypeRep() -- abstract and unforgeable
                      ,rnfTypeRep,typeRepFingerprint,appR
                      ,unsafeTemporaryMkTypeRep
                      ,module Data.TypeableT.Inst
                      ,typeOf
                      ,eqRR,eqRRHom
                      ,GetAppT(GA),getAppR,G1(G1),getR1,G2(G2),getR2,getFnR
                      -- * Thin wrappers around eqRRHom, for compatibility with 'Data.Typeable' in @base@
                      ,castR,cast,gcastR,gcast,gcastR1,gcast1,gcastR2,gcast2

                      -- * Providing the old interface. DEPRECATED
                      ,Typeable710
                      ,typeRep710
                      ,typeRepTo710
                      ,typeOf710, typeOf1710, typeOf2710, typeOf3710, typeOf4710, typeOf5710, typeOf6710, typeOf7710
                      ,Typeable1710, Typeable2710, Typeable3710, Typeable4710, Typeable5710, Typeable6710,Typeable7710
                      ,cast710
                      ,eqT710
                      ,gcast710
                      ,gcast1710
                      ,gcast2710
                      ,TypeRep710
                      ,typeRepFingerprint710
                      ,rnfTypeRep710
                      ,showsTypeRep710
                      ,TyCon710
                      ,tyConFingerprint710
                      ,tyConString710
                      ,tyConPackage710
                      ,tyConModule710
                      ,tyConName710
                      ,rnfTyCon710
                      ,mkTyCon3710
                      ,mkTyConApp710
                      ,mkAppTy710
                      ,mkFunTy710
                      ,splitTyConApp710
                      ,funResultTy710
                      ,typeRepTyCon710
                      ,typeRepArgs710
                      ) where

import Data.Proxy(Proxy(Proxy)) -- for compatibility shim
import Data.Type.Equality((:~:)(Refl))
import GHC.Fingerprint(Fingerprint(),fingerprintFingerprints)
import Unsafe.Coerce(unsafeCoerce)

import Data.TyConT
import safe Data.TypeableT.Internals
import safe Data.TypeableT.Inst

-- | Helper to fully evaluate 'TypeRep' for use as @NFData(rnf)@ implementation
rnfTypeRep :: TypeRep a -> ()
rnfTypeRep (TRCon c) = rnfTyCon c
rnfTypeRep (TRApp c a) = rnfTypeRep c `seq` rnfTypeRep a `seq` ()

-- | Create a constant-sized hash of a TypeRep, so users can implement extra consistency checks when transmitting data over a network, for instance.
typeRepFingerprint :: TypeRep a -> Fingerprint
typeRepFingerprint (TRCon c) = tyConFingerprint c
typeRepFingerprint (TRApp c a) = fingerprintFingerprints $ go c [typeRepFingerprint a]
  where go :: TypeRep a -> [Fingerprint] -> [Fingerprint]
        go (TRCon c') fprs = tyConFingerprint c' : fprs
        go (TRApp c' a') fprs = go c' $ typeRepFingerprint a' : fprs

-- we keep TypeRep abstract, but export smart constructors and destructors so we may include fingerprints in the data type, etc. at a later point
-- There is no reason for users to ever see a TyCon though
-- | Smart constructor for 'TypeRep'
appR :: TypeRep (a :: k -> k') -> TypeRep (b :: k) -> TypeRep (a b)
appR = TRApp

-- | Escape hatch so one can define new 'TypeRep's.
-- It is the user's responsability to ensure that the 'Id' is unique for each @a@, which should be a /Type Constructor/
-- i.e. @Maybe@, not @Maybe Bool@.
-- (Then the @instance (Typeable c, Typeable a) => Typeable (c a)@ will take care of applications.)
-- This will be removed when GHC supports deriving 'Data.TypeableT.Typeable'.
unsafeTemporaryMkTypeRep :: Id -- ^ A 3-tuple of strings: (package , module , name)
                         -> TypeRep (a :: k)
unsafeTemporaryMkTypeRep = TRCon . unsafeTemporaryMkTyCon

-- | GADT for return type of 'getApp'
-- Fully poly-kinded: @GetAppT :: k -> *@.
-- At the moment you'll need an 'unsafeCoerce' if you use this function,
-- because the kind variables in 'GA' are existentially bound.
-- But it's still useful, to take apart type applications when you
-- know what you are doing; and the need for 'unsafeCoerce' will vanish
-- when we get kind equalities.
data GetAppT (a :: k) where
  GA :: TypeRep (a :: k1 -> k2) -> TypeRep (b :: k1) -> GetAppT (a b)

instance Show (GetAppT a) where
  showsPrec d (GA a b) = showParen (d > p) $ showString "GA " . showsPrec (p+1) a . showString " " . showsPrec (p+1) b
    where p = 10

-- | Smart destructor, as a converse to 'appR'
getAppR :: TypeRep (a :: k) -> Maybe (GetAppT a)
getAppR (TRApp a b) = Just $ GA a b
getAppR _ = Nothing

-- | @typeOf x@ is a runtime reflection of @x@'s type.
typeOf :: Typeable a => (a :: *) -> TypeRep a
typeOf _ = typeRep


-- Can actually give :~~: evidence with Richard's branch
-- | Compare two 'TypeRep's for equality, even if differently kinded.
-- This returns a Boolean, which is not hugely useful, as we get no
-- static type information.
eqRR :: TypeRep (a::k1) -> TypeRep (b::k2) -> Bool
eqRR (TRCon a) (TRCon b) = eqTyCon a b
eqRR (TRApp c a) (TRApp d b) = eqRR c d && eqRR a b
eqRR _ _ = False


-- | Compare two 'TypeRep's for equality, and return /evidence/.
-- Unfortunately, currently GHC cannot handle kind-hetrogenous
-- equalities, so the 'TypeRep's must have the same kind.
eqRRHom :: TypeRep (a::k) -> TypeRep (b::k) -> Maybe (a :~: b)
eqRRHom (TRCon a) (TRCon b) = eqTyConHom a b
eqRRHom (TRApp c a) (TRApp d b) = if eqRR c d && eqRR a b  -- note that c :: TypeRep (a1::k1->k) and d :: TypeRep (a2::k2->k), so can't recurse to eqRRHom c d and eqRRHom a b, but could with Richard's branch!
                                  then Just $ unsafeCoerce Refl
                                  else Nothing
eqRRHom _ _ = Nothing


-- | GADT for return type of 'getR1'
-- Poly-kinded: @G1 (k -> k') -> k' -> *@.
data G1 (c :: k -> k') (a :: k') where
  G1 :: TypeRep (a :: k) -> G1 (c :: k -> k') (c a)
instance Show (G1 c a) where
  showsPrec d (G1 a) = showParen (d > p) $ showString "G1 " . showsPrec (d+1) a
    where p = 10

-- could use getApp rather than pattern matching, but I think this is clearer
-- | @getR1 c x@ trys to strip a @c :: k1 -> k2@ off the beginning of @x@'s 'TypeRep'
-- i.e. c has to be a type constructor of arity 1
-- We provide this seperately to getApp as this needs an unsafeCoerce in it's implementation.
--
-- >>> getR1 (typeRepMaybe) $ typeOf $ Just True
-- Just (G1 Bool)
--
-- >>> getR1 (typeRepList) $ typeOf $ Just True
-- Nothing
getR1 :: TypeRep (ct :: k1 -> k) -> TypeRep (c'at :: k) -> Maybe (G1 ct c'at)
-- being very explicit with kinds:
--getR1 (c :: TypeRep (ct :: k1 -> k))
--      (TRApp (c' :: TypeRep (c't :: k2 -> k))
--             (a :: TypeRep (at :: k2)))
getR1 c (TRApp c' a)
  = if eqRR c c'
-- We want to say ``G1 a'' here, but that has type G1 ct (ct at), and we need G1 ct c'at
-- We know c'at ~ c't at and eqRR c c' == True, but NOT that c't ~ ct, or k2 ~ k1, though they must be
--
-- Note that we can't use a gcast* in any form, as the two G1's are _different_ (kind polymorphic)
    then Just (unsafeCoerce $ G1 a :: G1 ct c'at)
    else Nothing
getR1 _ _ = Nothing


-- We indeed need to index G2 by the constructor c, to give GHC enough info
-- to enable us to write dynApply (see Dynamic.hs)
-- We dismantle a `TypeRep a` as `Maybe (G2 tsrc ttgt)`
-- and then compare `eqRRHom tsrc tx` that the source's type lines
-- up with the argument's type, but we need `tsrc :: TypeRep src :: *` for this,
-- but GHC won't know this unless we inform it we have dismantled a `(->)` constructor!
-- | GADT for return type of 'getR2' and 'getFnR'.
-- Poly-kinded: @G2 (k1 -> k2 -> k3) -> k3 -> *@.
data G2 (c :: k1 -> k2 -> k3) (a :: k3) where
  G2 :: TypeRep (a :: k1) -> TypeRep (b :: k2) -> G2 (c :: k1 -> k2 -> k3) (c a b)
instance Show (G2 c a) where
  showsPrec d (G2 a b) = showParen (d > p) $ showString "G2 " . showsPrec (d+1) a . showString " " . showsPrec (d+1) b
    where p = 10

-- now clearer to use getAppR
-- | @getT1 c x@ trys to strip a @c :: k1 -> k2 -> k3@ off the beginning of @x@'s 'TypeRep'
-- i.e. c has to be a type constructor of arity 2
--
-- >>> getR2 (typeRepArr) $ typeOf $ Just True
-- Nothing
---
--- >>> getR2 (typeRepArr) $ typeOf not
-- Just (G2 Bool Bool)
getR2 :: TypeRep (c :: k2 -> k1 -> k) -> TypeRep (a :: k) -> Maybe (G2 c a)
getR2 c t = do GA t' b <- getAppR t
               GA c' a <- getAppR t'
               if eqRR c c'
                 then Just (unsafeCoerce $ G2 a b :: G2 c a)
                 else Nothing

--TODO: how many of these functions to provide?

-- | Convenience function: 'getR2' specialised to the function type constructor (->)
getFnR :: TypeRep (a :: *) -> Maybe (G2 (->) a)
getFnR = getR2 typeRepArr


-- | Type-safe cast
castR :: TypeRep (a :: *) -> TypeRep (b :: *) -> a -> Maybe b
castR ta tb x = fmap (\Refl -> x) $ eqRRHom ta tb

-- | Type-safe cast
cast :: (Typeable (a :: *), Typeable (b :: *)) => a -> Maybe b
cast = castR typeRep typeRep

-- | A flexible variation parameterised in a type constructor
gcastR :: TypeRep (a :: k) -> TypeRep (b :: k) -> c a -> Maybe (c b)
gcastR ta tb x = fmap (\Refl -> x) $ eqRRHom ta tb

-- | A flexible variation parameterised in a type constructor
gcast :: (Typeable (a :: k), Typeable (b :: k)) => c a -> Maybe (c b)
gcast = gcastR typeRep typeRep

-- | Cast over @k1 -> k2@
gcastR1 :: TypeRep (t :: k1 -> k2) -> TypeRep (t' :: k1 -> k2) -> c (t a) -> Maybe (c (t' a))
gcastR1 t t' x = fmap (\Refl -> x) $ eqRRHom t t'

-- | Cast over @k1 -> k2@
gcast1 :: (Typeable (t :: k1 -> k2), Typeable (t' :: k1 -> k2)) => c (t a) -> Maybe (c (t' a))
gcast1 = gcastR1 typeRep typeRep

-- | Cast over @k1 -> k2 -> k3@
gcastR2 :: TypeRep (t :: k1 -> k2 -> k3) -> TypeRep (t' :: k1 -> k2 -> k3) -> c (t a b) -> Maybe (c (t' a b))
gcastR2 t t' x = fmap (\Refl -> x) $ eqRRHom t t'

-- | Cast over @k1 -> k2 -> k3@
gcast2 :: (Typeable (t :: k1 -> k2 -> k3), Typeable (t' :: k1 -> k2 -> k3)) => c (t a b) -> Maybe (c (t' a b))
gcast2 = gcastR2 typeRep typeRep


------------------------------------------------------------
-- Compatibility shim for Typeable as in base at GHC 7.10 --
------------------------------------------------------------
-- | An abstract representation of a type constructor.  'TyCon710' objects can
-- be built using 'mkTyCon3710'.
data TyCon710 where
  TyCon710 :: TyCon a -> TyCon710

instance Show TyCon710 where
  show (TyCon710 c) = show c

instance Eq TyCon710 where
  t == t' = tyConFingerprint710 t == tyConFingerprint710 t'

-- | A concrete representation of a (monomorphic) type.  'TypeRep710'
-- supports reasonably efficient equality.
-- TODO: NB: verbatim doc from @base@ - we don't have efficient equality yet!
data TypeRep710 = TypeRep710 TyCon710 [TypeRep710] -- abstract

-- NOTE: this changes printed string from current base (for (->), [] etc...)
instance Show TypeRep710 where
  showsPrec _ (TypeRep710 c []) = shows c
  showsPrec d (TypeRep710 c as) = showParen (d > p) $ shows' (showsPrec d c "") as
    where p = 10
          shows' t [] = showString t
          shows' t [a] = showString t . showString " " . showsPrec (p+1) a
          shows' t (b:bs) = shows'  ("(" ++ t ++ " " ++ showsPrec (p+1) b ")") bs

instance Eq TypeRep710 where
  t == t' = typeRepFingerprint710 t == typeRepFingerprint710 t'

-- | We can drop the type-index.
typeRepTo710 :: TypeRep a -> TypeRep710
typeRepTo710 (TRCon c) = TypeRep710 (TyCon710 c) []
typeRepTo710 (TRApp a b) = case typeRepTo710 a
                           of TypeRep710 c as -> TypeRep710 c (as ++ [typeRepTo710 b])

class Typeable a => Typeable710 a where
  typeRep710' :: Proxy a -> TypeRep710

instance Typeable a => Typeable710 a where
  typeRep710' _ = typeRepTo710 (typeRep :: TypeRep a)

-- | Takes a value of type @a@ and returns a concrete representation
-- of that type.
typeRep710 :: forall proxy a. Typeable710 a => proxy a -> TypeRep710
typeRep710 _ = typeRep710' (Proxy :: Proxy a)

-- Backwards compatibility for older @base@ versions
typeOf710 :: forall a. Typeable710 a => a -> TypeRep710
typeOf710 _ = typeRep710 (Proxy :: Proxy a)
typeOf1710 :: forall t a. Typeable710 t => t a -> TypeRep710
typeOf1710 _ = typeRep710 (Proxy :: Proxy t)
typeOf2710 :: forall t a b. Typeable710 t => t a b -> TypeRep710
typeOf2710 _ = typeRep710 (Proxy :: Proxy t)
typeOf3710 :: forall t a b c. Typeable710 t => t a b c -> TypeRep710
typeOf3710 _ = typeRep710 (Proxy :: Proxy t)
typeOf4710 :: forall t a b c d. Typeable710 t => t a b c d -> TypeRep710
typeOf4710 _ = typeRep710 (Proxy :: Proxy t)
typeOf5710 :: forall t a b c d e. Typeable710 t => t a b c d e -> TypeRep710
typeOf5710 _ = typeRep710 (Proxy :: Proxy t)
typeOf6710 :: forall t a b c d e f. Typeable710 t => t a b c d e f -> TypeRep710
typeOf6710 _ = typeRep710 (Proxy :: Proxy t)
typeOf7710 :: forall t a b c d e f g. Typeable710 t => t a b c d e f g -> TypeRep710
typeOf7710 _ = typeRep710 (Proxy :: Proxy t)

type Typeable1710 a = Typeable710 a
type Typeable2710 a = Typeable710 a
type Typeable3710 a = Typeable710 a
type Typeable4710 a = Typeable710 a
type Typeable5710 a = Typeable710 a
type Typeable6710 a = Typeable710 a
type Typeable7710 a = Typeable710 a

-- | The type-safe cast operation
cast710 :: forall a b. (Typeable710 a, Typeable710 b) => a -> Maybe b
cast710 x = castR typeRep typeRep x

-- | Extract a witness of equality of two types
eqT710 :: forall a b. (Typeable710 a, Typeable710 b) => Maybe (a :~: b)
eqT710 = eqRRHom typeRep typeRep

-- | A flexible variation parameterised in a type constructor
gcast710 :: forall a b c. (Typeable710 a, Typeable710 b) => c a -> Maybe (c b)
gcast710 = gcastR typeRep typeRep

-- | Cast over @k1 -> k2@
gcast1710 :: forall c t t' a. (Typeable710 t, Typeable710 t') => c (t a) -> Maybe (c (t' a))
gcast1710 = gcastR1 typeRep typeRep

-- | Cast over @k1 -> k2 -> k3@
gcast2710 :: forall c t t' a b. (Typeable710 t, Typeable710 t') => c (t a b) -> Maybe (c (t' a b))
gcast2710 = gcastR2 typeRep typeRep

-- TODO: cache the fingerprints in the values?
-- | Observe the 'Fingerprint' of a type representation
typeRepFingerprint710 :: TypeRep710 -> Fingerprint
typeRepFingerprint710 (TypeRep710 c []) = tyConFingerprint710 c
typeRepFingerprint710 (TypeRep710 c as) = fingerprintFingerprints
                                          $ tyConFingerprint710 c : map typeRepFingerprint710 as

-- | Helper to fully evaluate 'TypeRep710' for use as @NFData(rnf)@ implementation
rnfTypeRep710 :: TypeRep710 -> ()
rnfTypeRep710 (TypeRep710 c as) = rnfTyCon710 c `seq` go as
  where go [] = ()
        go (b:bs) = rnfTypeRep710 b `seq` go bs

showsTypeRep710 :: TypeRep710 -> ShowS
showsTypeRep710 = shows

-- TODO: cache the fingerprints in the values?
tyConFingerprint710 :: TyCon710 -> Fingerprint
tyConFingerprint710 (TyCon710 c) = tyConFingerprint c

-- | Observe string encoding of a type representation
tyConString710 :: TyCon710 -> String
tyConString710 (TyCon710 c) = let (_,_,n) = tyConId c
                              in n

tyConPackage710 :: TyCon710 -> String
tyConPackage710 (TyCon710 c) = let (p,_,_) = tyConId c
                               in p

tyConModule710 :: TyCon710 -> String
tyConModule710 (TyCon710 c) = let (m,_,_) = tyConId c
                              in m

tyConName710 :: TyCon710 -> String
tyConName710 (TyCon710 c) = let (_,_,n) = tyConId c
                            in n

-- | Helper to fully evaluate 'TyCon710' for use as @NFData(rnf)@ implementation
rnfTyCon710 :: TyCon710 -> ()
rnfTyCon710 (TyCon710 c) = rnfTyCon c

-- | Builds a 'TyCon710' object representing a type constructor.  An
-- implementation should ensure that the following holds:
--
-- >  A==A' ^ B==B' ^ C==C' ==> mkTyCon710 A B C == mkTyCon710 A' B' C'
--

--
mkTyCon3710 :: String    -- ^ package name
            -> String    -- ^ module name
            -> String    -- ^ the name of the type constructor
            -> TyCon710  -- ^ A unique 'TyCon710' object
mkTyCon3710 p m n = TyCon710 $ unsafeTemporaryMkTyCon (p,m,n)

-- | Applies a monomorphic type constructor to a sequence of types
mkTyConApp710 :: TyCon710 -> [TypeRep710] -> TypeRep710
mkTyConApp710 = TypeRep710

-- | Adds a TypeRep710 argument to a TypeRep710.
mkAppTy710 :: TypeRep710 -> TypeRep710 -> TypeRep710
mkAppTy710 (TypeRep710 c as) b = TypeRep710 c (as ++ [b])

-- a TyCon710 for (->)
-- only used internally, in mkFunTy710 and funResultTy710
funTc :: TyCon710
funTc = go typeRepArr
  where go (TRCon tcArr) = TyCon710 tcArr
        go _ = error "We know typeRepArr is a TRCon, so this cannot happen"

-- | A special case of 'mkTyConApp710', which applies the function
-- type constructor to a pair of types.
mkFunTy710 :: TypeRep710 -> TypeRep710 -> TypeRep710
mkFunTy710 f a = mkTyConApp710 funTc [f,a]

-- | Splits a type constructor application.
-- Note that if the type construcotr is polymorphic, this will
-- not return the kinds that were used.
splitTyConApp710 :: TypeRep710 -> (TyCon710, [TypeRep710])
splitTyConApp710 (TypeRep710 c as) = (c,as)

-- | Applies a type to a function type.  Returns: @'Just' u@ if the
-- first argument represents a function of type @t -> u@ and the
-- second argument represents a function of type @t@.  Otherwise,
-- returns 'Nothing'.
funResultTy710 :: TypeRep710 -> TypeRep710 -> Maybe TypeRep710
funResultTy710 trFun trArg
  = case splitTyConApp710 trFun of
      (tc, [t1,t2]) | tc == funTc && t1 == trArg -> Just t2
      _ -> Nothing

-- | Observe the type constructor of a type representation
typeRepTyCon710 :: TypeRep710 -> TyCon710
typeRepTyCon710 = fst . splitTyConApp710

-- | Observe the argument types of a type representation
typeRepArgs710 :: TypeRep710 -> [TypeRep710]
typeRepArgs710 = snd . splitTyConApp710


{-# DEPRECATED Typeable710
              ,typeRep710
              ,typeOf710, typeOf1710, typeOf2710, typeOf3710, typeOf4710, typeOf5710, typeOf6710, typeOf7710
              ,Typeable1710, Typeable2710, Typeable3710, Typeable4710, Typeable5710, Typeable6710,Typeable7710
              ,cast710
              ,eqT710
              ,gcast710
              ,gcast1710
              ,gcast2710
              ,TypeRep710
              ,typeRepFingerprint710
              ,rnfTypeRep710
              ,showsTypeRep710
              ,TyCon710
              ,tyConFingerprint710
              ,tyConString710
              ,tyConPackage710
              ,tyConModule710
              ,tyConName710
              ,rnfTyCon710
              ,mkTyCon3710
              ,mkTyConApp710
              ,mkAppTy710
              ,mkFunTy710
              ,splitTyConApp710
              ,funResultTy710
              ,typeRepTyCon710
              ,typeRepArgs710
      "You should start migrating away from the '710' interface to the non-suffixed interface"  #-}
