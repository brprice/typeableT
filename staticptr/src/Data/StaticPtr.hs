-- | Stability: experimental
--
-- Support for static pointers: pointers to values that are known at compile time.

{-# LANGUAGE Safe #-}
{-# LANGUAGE GADTs , FlexibleInstances #-}

module Data.StaticPtr (Dict(Dict)
                      ,StaticPtr()
                      ,deRefStaticPtr
                      ,putSDynStaticPtr
                      ,putStaticPtr
                      ,getSDynStaticPtr
                      ,getStaticPtr
                      ,Static()
                      ,staticMono
                      ,staticApp
                      ,deRefStatic
                      ,putSDynStatic
                      ,putStatic
                      ,getSDynStatic
                      ,getStatic
                      ,staticNot
                      ,staticTrue
                      ,staticBsNot
                      ,staticDecBool
                      ,staticDecInt
                      ,staticSucc
                      ,staticAdd
                      ,staticPredBS
                      ,staticDBBool
                      ,staticDBInt
                      ,staticDTBool
                      ,staticDTInt
                      ,sExtractDecode
                      ,sMaybeDB
                      ,sMaybeDT
                      ,sListDB
                      ,sListDT
                      ,sReverse
                      ) where

import safe Data.Binary(Binary(put,get),Put(),Get(),putWord8,getWord8)

import safe Data.TypeableT(TypeRep(),Typeable(typeRep),G1(G1),getR1,G2(G2),getFnR,gcastR)
import safe Data.DynamicT(SDynamic(SDynamic))

import safe Data.StaticPtr.Internals
import safe Data.StaticPtr.SPT

--I don't want to export the PolyPtr ctor, so for consistency, I've exported functions and not ctors for the other two
-- | Smart constructor for 'Static'
staticMono :: StaticPtr a -> Static a
staticMono = MonoPtr

-- | Smart constructor for 'Static'
staticApp :: Static (a -> b) -> Static a -> Static b
staticApp = StaticApp

-- | Extract a value from a 'StaticPtr'
deRefStaticPtr :: StaticPtr a -> a
deRefStaticPtr (StaticPtr _ a) = a

-- | Extract a value from a 'Static'
deRefStatic :: Static a -> a
deRefStatic (MonoPtr p) = deRefStaticPtr p
deRefStatic (PolyPtr _ _ targ a) = a (deRefStatic targ)
deRefStatic (StaticApp f x) = deRefStatic f (deRefStatic x)

lookupSPT :: StaticName -> Maybe (SDynamic StaticPtr)
lookupSPT n | 0<=n && n<length spt = Just $ spt !! n
            | otherwise = Nothing

lookupPPT :: StaticName -> Static (Dict (Typeable a)) -> Maybe (SDynamic Static) -- Note that a :: *
lookupPPT n ta | 0 <= n && n < length ppt
  = case ppt !! n
    of PolyTblEnt t f -> (\Dict -> Just $ SDynamic typeRep (PolyPtr n t ta f)) $ typeableConstraint t (deRefStatic ta)
               | otherwise = Nothing

instance Binary (SDynamic StaticPtr) where
  put = putSDynStaticPtr
  get = getSDynStaticPtr

instance Typeable a => Binary (StaticPtr a) where
  put = putStaticPtr
  get = getStaticPtr typeRep

instance Binary (SDynamic Static) where
  put = putSDynStatic
  get = getSDynStatic

instance Typeable a => Binary (Static a) where
  put = putStatic
  get = getStatic typeRep

-- | Serialise a @'SDynamic' 'StaticPtr'@. See also 'decode' (default definition in terms of 'putSDynStaticPtr') from the 'Binary' instance of @'SDynamic' 'StaticPtr'@
putSDynStaticPtr :: SDynamic StaticPtr -> Put
putSDynStaticPtr (SDynamic _ s) = putStaticPtr s

-- | Serialise a 'StaticPtr'. See also 'decode' (default definition in terms of 'putStaticPtr') from the 'Binary' instance of 'StaticPtr'
putStaticPtr :: StaticPtr a -> Put
putStaticPtr (StaticPtr n _) = put n

-- | Serialise a @'SDynamic' 'Static'@. See also 'decode' (default definition in terms of 'putSDynStatic') from the 'Binary' instance of @'SDynamic' 'Static'@
putSDynStatic :: SDynamic Static -> Put
putSDynStatic (SDynamic _ s) = putStatic s

-- | Serialise a 'Static'. See also 'decode' (default definition in terms of 'putStatic') from the 'Binary' instance of 'Static'
putStatic :: Static a -> Put
putStatic (MonoPtr p) = putWord8 0 >> putStaticPtr p
putStatic (PolyPtr n _ typ _) = putWord8 1 >> put n >> putStatic typ
putStatic (StaticApp f x) = putWord8 2 >> putStatic f >> putStatic x

-- | Utility function to make working with the 'Maybe' monad inside the 'Get' monad nicer
maybeToGet :: String -> Maybe a -> Get a
maybeToGet err Nothing = fail err
maybeToGet _ (Just x) = return x

-- | Deserialise a 'StaticPtr' to a 'SDynamic' value, without having to know the type.
getSDynStaticPtr :: Get (SDynamic StaticPtr)
getSDynStaticPtr = do name <- get
                      maybeToGet "getSDynStaticPtr: lookup failed" $ lookupSPT name

-- | Deserialise a 'StaticPtr'. See also 'encode' (default definition in terms of 'getStaticPtr') from the 'Binary' instance of 'StaticPtr'
getStaticPtr :: TypeRep a -> Get (StaticPtr a)
getStaticPtr t = do SDynamic ta sa <- getSDynStaticPtr
                    maybeToGet ("getStaticPtr: dynamic typecheck failed (had "
                                ++ show t ++ " and " ++ show ta ++ ")")
                      $ gcastR ta t sa

-- | Deserialise a 'Static' to a 'SDynamic' value, without having to know the type.
getSDynStatic :: Get (SDynamic Static)
getSDynStatic = getWord8 >>= go
  where go 0 = do SDynamic t p <- getSDynStaticPtr -- MonoPtr case
                  return $ SDynamic t (MonoPtr p)
        go 1 = do name <- get :: Get StaticName  -- I don't undestand why we need a type sig? -- PolyPtr case
                  (SDynamic ttyp typ) <- getSDynStatic
                  G1 ttyp' <- maybeToGet "getSDynStatic (poly case): static Dict (Typeable _) arg is not actually a Dict _"
                                       $ getR1 typeRepDict ttyp
                  G1 _ <- maybeToGet "getSDynStatic (poly case): static Dict (Typeable _) arg is not actually a Dict (Typeable _)"
                                   $ getR1 (typeRepTypeable :: TypeRep (Typeable {-:: * -> Constraint-})) ttyp'
                  maybeToGet "getSDynStatic (poly case): lookup failed" $ lookupPPT name typ
        go 2 = do SDynamic tf f <- getSDynStatic -- StaticApp case
                  SDynamic tx x <- getSDynStatic
                  G2 tsrc ttgt <- maybeToGet "getSDynStatic (app case): not a function" $ getFnR tf
                  x' <- maybeToGet "getSDynStatic (app case): function expects different type to argument given"
                        $ gcastR tx tsrc x
                  return $ SDynamic ttgt (StaticApp f x')
        go _ = fail "getSDynStaticPtr: bad encoding"


-- | Deserialise a 'Static'. See also 'encode' (default definition in terms of 'getStatic') from the 'Binary' instance of 'Static'
getStatic :: TypeRep a -> Get (Static a)
getStatic t = do SDynamic ta sa <- getSDynStatic
                 maybeToGet ("getStatic: dynamic typecheck failed (had "
                             ++ show t ++ " and " ++ show ta ++ ")")
                   $ gcastR ta t sa
