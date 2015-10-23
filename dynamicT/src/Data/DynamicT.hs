-- | Stability: experimental
--
-- Basic support for Dynamic (monomorphic) types

{-# LANGUAGE Safe #-}
{-# LANGUAGE GADTs  #-}

{- completely untrusted code -}

module Data.DynamicT(Dynamic(Dynamic)   -- NB: not abstract. Pattern-matching on this is fine. (Change from current Dynamic in base)
                    ,toDynR,toDyn
                    ,fromDynamicR,fromDynamic
                    ,fromDynR,fromDyn
                    ,dynApply
                    ,SDynamic(SDynamic)  -- NB: not abstract. Pattern-matching on this is fine.
                    ,toSDynR,toSDyn
                    ,fromSDynamicR,fromSDynamic
                    ,fromSDynR,fromSDyn
                    ) where

import Data.TypeableT(TypeRep(),Typeable(typeRep),G2(G2),getFnR,castR,gcastR)

import Data.Maybe(fromMaybe)

-- | A value of type 'Dynamic' is a object along with a run-time
-- representation of it's type.
-- The type is not abstract: pattern matching on it is fine.
data Dynamic where
  Dynamic :: TypeRep a -> a -> Dynamic


-- | Convert a 'Typeable' value to a 'Dynamic'
toDynR :: TypeRep a -> a -> Dynamic
toDynR = Dynamic

-- | Like 'toDynR', but with class constraint, not a 'TypeRep' argument
toDyn :: Typeable a => a -> Dynamic
toDyn = toDynR typeRep

-- | Given a potential type, try to extract that type of object from a 'Dynamic'
fromDynamicR :: TypeRep a -> Dynamic -> Maybe a
fromDynamicR t (Dynamic ta a) = castR ta t a

-- | Like 'fromDynamicR', but with class constraint, not a 'TypeRep' argument
fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic = fromDynamicR typeRep

-- | Like 'fromDynamicR', but with a default value to return instead of 'Nothing'
fromDynR :: TypeRep a -> Dynamic -> a -> a
fromDynR t v def = fromMaybe def $ fromDynamicR t v

-- | Like 'fromDynamic', but with a default value to return instead of 'Nothing'
-- Or, like 'fromDynR', but with class constraint, not a 'TypeRep' argument
fromDyn :: Typeable a => Dynamic -> a -> a
fromDyn = fromDynR typeRep

instance Show Dynamic where
  show (Dynamic tx _) = "Dynamic of type " ++ show tx

-- | Attempt to apply a function in a 'Dynamic' to another 'Dynamic'.
-- Returns 'Nothing' if there is a type mismatch
dynApply :: Dynamic -> Dynamic -> Maybe Dynamic
dynApply (Dynamic tf f) (Dynamic tx x) = do G2 tsrc ttgt <- getFnR tf
                                            x' <- castR tx tsrc x
                                            return $ Dynamic ttgt $ f x'

-- | 'SDynamic' is a variant of 'Dynamic', a type @a@ with a value of type @s a@
-- In effect, 's' gives the statically-known shape of the dynamic value's type,
-- while 'a' is the statically-unknown part, for which we get
-- a 'TypeRep' when we pattern-match on it.
-- Thus, @Dynamic ~ SDynamic Id@
-- The type is not abstract: pattern matching on it is fine.
data SDynamic s where
  SDynamic :: TypeRep a -> s a -> SDynamic s

-- | Like 'toDynR', but for 'SDynamic'
toSDynR :: TypeRep a -> s a -> SDynamic s
toSDynR = SDynamic

-- | Like 'toDyn', but for 'SDynamic'
toSDyn :: Typeable a => s a -> SDynamic s
toSDyn = toSDynR typeRep

-- | Like 'fromDynamicR', but for 'SDynamic'
fromSDynamicR :: TypeRep a -> SDynamic s -> Maybe (s a)
fromSDynamicR t (SDynamic ta sa) = gcastR ta t sa

-- | Like 'fromDynamic', but for 'SDynamic'
fromSDynamic :: Typeable a => SDynamic s -> Maybe (s a)
fromSDynamic = fromSDynamicR typeRep

-- | Like 'fromDynR', but for 'SDynamic'
fromSDynR  :: TypeRep a -> SDynamic s -> s a -> s a
fromSDynR t v def = fromMaybe def $ fromSDynamicR t v

-- | Like 'fromSDynamic', but with a default value to return instead of 'Nothing'
-- Or, like 'fromSDynR', but with class constraint, not a 'TypeRep' argument
fromSDyn :: Typeable a => SDynamic s -> s a -> s a
fromSDyn = fromSDynR typeRep
