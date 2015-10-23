-- | Stability: experimental
--
-- An implementation of serialisable closures, to enable distributed programming.

{-# LANGUAGE Safe #-}
{-# LANGUAGE GADTs , FlexibleInstances , TypeFamilies #-}

module Control.DistributedClosure (Closure()
                                  ,unclosure
                                  ,closureSP
                                  ,closureS
                                  ,closureEnc
                                  ,closureApp
                                  ,closurePure
                                  ,putSDynClosure
                                  ,putClosure
                                  ,getSDynClosure
                                  ,getClosure
                                  ,Serializable(..)
                                  ,closureRemoteTable
                                  ) where

import safe Data.Binary(Binary(put,get),Put(),Get(),decode,encode,putWord8,getWord8)
import safe Data.ByteString.Lazy(ByteString())

import safe Data.TypeableT(TypeRep(),unsafeTemporaryMkTypeRep,Typeable(typeRep),G2(G2),getFnR,gcastR)
import safe Data.DynamicT(SDynamic(SDynamic))
import safe Data.StaticPtr (Dict(Dict),Tag(PolyTag,typeableConstraint),PolyTblEnt(PolyTblEnt)
                           ,RemoteTable,initRemoteTable
                           ,registerStaticPoly,staticPolyPtrAt
                           ,StaticPtr(),deRefStaticPtr,putStaticPtr,getSDynStaticPtr
                           ,Static(),staticApp,deRefStatic,putStatic,getSDynStatic
                           )


-- | A minimal remote table containing just what closures require
closureRemoteTable :: RemoteTable
closureRemoteTable = registerStaticPoly "extractDecode" (PolyTblEnt ExtractDecodeTag (\_ Dict -> decode))
                     $ initRemoteTable

typeRepBinary :: TypeRep Binary
typeRepBinary = unsafeTemporaryMkTypeRep ("binar_EKE3c9Lmxb3DQpU0fPtru6","Data.Binary.Class","Get")
instance Typeable Binary where
  typeRep = typeRepBinary

data ExtractDecodeTag = ExtractDecodeTag deriving Show
instance Tag ExtractDecodeTag where
  type PolyTag ExtractDecodeTag a = Dict (Binary a) -> ByteString -> a
  typeableConstraint _ Dict = Dict
sExtractDecode :: Static (Dict (Typeable a)) -> Static (PolyTag ExtractDecodeTag a)
sExtractDecode = staticPolyPtrAt closureRemoteTable ExtractDecodeTag "extractDecode"


-- | Abstract type representing closures.
-- See 'closureSP', 'closureS', 'closureEnc' and 'closureApp' for smart constructors, and also 'closurePure'.
-- For instance
-- @'closureApp' ('closureSP' f) ('closurePure' x)@
-- (for @f@ a 'StaticPtr' of type @a->b@, and @x::a@)
-- represents a closure of the function @f@, capturing the variable @x@.
--
-- Note, however, that 'closurePure' can be quite inefficient, as it has to serialise a decoder,
-- which may well be a fairly large 'Static'.
-- It is more efficient, when possible, to bake the decoder into 'f'.
-- i.e., (once GHC supports the @static@ keyword in the format we need) instead of
--
-- >>> f x = (closureS $ static not) `closureApp` (closurePure x)
-- one could do
--
-- >>> f x = (closureS $ static (not . (decode :: ByteString -> Bool))) `closureApp` (closureEnc $ encode x)
data Closure a where
  StaticPtr :: a -> StaticPtr a -> Closure a
  Static :: a -> Static a -> Closure a
  Encoded :: ByteString -> Closure ByteString
  App :: b -> Closure (a -> b) -> Closure a -> Closure b

instance Show a => Show (Closure a) where
  showsPrec d c = showParen (d>p) $ showString "Closure {val: " . showsPrec (p+1) (unclosure c) . showString " rep: " . showsPrec' (p+1) c . showString " }"
    where p = 10
          showsPrec' :: Int -> Closure a -> String -> String
          showsPrec' d' (StaticPtr _ sp) = showParen (d' > p) $ showString "StaticPtr " . showsPrec (p+1) sp
          showsPrec' d' (Static _ s) = showParen (d' > p) $ showString "Static " . showsPrec (p+1) s
          showsPrec' d' (Encoded bs) = showParen (d' > p) $ showString "Encoded " . showsPrec (p+1) bs
          showsPrec' d' (App _ cf ca) = showParen (d' > p) $ showString "App " . showsPrec' (p+1) cf . showString " " . showsPrec' (p+1) ca



-- | Extract the value of a 'Closure'
unclosure :: Closure a -> a
unclosure (StaticPtr a _) = a
unclosure (Static a _) = a
unclosure (Encoded bs) = bs
unclosure (App fa _ _) = fa

-- | Convert a 'StaticPtr' into a 'Closure'
closureSP :: StaticPtr a -> Closure a
closureSP sp = StaticPtr (deRefStaticPtr sp) sp

-- | Convert a 'Static' into a 'Closure'
closureS :: Static a -> Closure a
closureS s = Static (deRefStatic s) s

-- | Convert a 'ByteString' into a 'Closure'
closureEnc :: ByteString -> Closure ByteString
closureEnc = Encoded

-- | Smart constructor: the application of two 'Closure's is a 'Closure'
closureApp :: Closure (a -> b) -> Closure a -> Closure b
closureApp cf ca = App (unclosure cf $ unclosure ca) cf ca

-- | Serialise a @'SDynamic' 'Closure'@. See also 'decode' (default definition in terms of 'putSDynClosure') from the 'Binary' instance of @'Dynamic' 'Closure'@
putSDynClosure :: SDynamic Closure -> Put
putSDynClosure (SDynamic _ c) = putClosure c

-- | Serialise a 'Closure'. See also 'decode' (default definition in terms of 'putClosure') from the 'Binary' instance of 'Closure'
putClosure :: Closure a -> Put
putClosure (StaticPtr _ sa) = putWord8 0 >> putStaticPtr sa
putClosure (Static _ sa) = putWord8 1 >> putStatic sa
putClosure (Encoded bs) = putWord8 2 >> put bs
putClosure (App _ cf ca) = putWord8 3 >> putClosure cf >> putClosure ca

-- | Utility function to make working with the 'Maybe' monad inside the 'Get' monad nicer
maybeToGet :: String -> Maybe a -> Get a
maybeToGet err Nothing = fail err
maybeToGet _ (Just x) = return x

-- | Deserialise a 'Closure' to a 'SDynamic' value, without having to know the type.
getSDynClosure :: RemoteTable -> Get (SDynamic Closure)
getSDynClosure rt = getWord8 >>= go
  where go 0 = do SDynamic tsp sp <- getSDynStaticPtr rt -- StaticPtr case
                  return $ SDynamic tsp $ closureSP sp
        go 1 = do SDynamic tsp s <- getSDynStatic rt -- Static case
                  return $ SDynamic tsp $ closureS s
        go 2 = do bs <- get -- Encoded case
                  return $ SDynamic typeRep $ Encoded bs
        go 3 = do SDynamic tf cf <- getSDynClosure rt -- App case
                  G2 src tgt <- maybeToGet "getSDynClosure - App ctor not applied to a function" $ getFnR tf
                  SDynamic ta ca <- getSDynClosure rt
                  ca' <- maybeToGet "getSDynClosure - App function input and argument type mismatch" $ gcastR ta src ca
                  return $ SDynamic tgt $ closureApp cf ca'
        go _ = fail "getSDynClosure: bad encoding"

-- | Deserialise a 'Closure'. See also 'encode' (default definition in terms of 'getClosure') from the 'Binary' instance of 'Closure'
getClosure :: RemoteTable -> TypeRep a -- ^ The type to deserialise at
           -> Get (Closure a)
getClosure rt t = do SDynamic tc c <- getSDynClosure rt
                     maybeToGet ("getClosure: dynamic typecheck failed (had "
                                 ++ show t ++ " and " ++ show tc ++ ")")
                       $ gcastR tc t c

-- We can't provide Binary instances, as need to differ depending on RemoteTable.
-- We force users to provide (orphan) instances

-- | A class for those types for which we have /static/ evidence of their 'Binary' and 'Typeable'
-- nature, and so can serialise them (via 'closurePure')
class (Binary a, Typeable a) => Serializable a where
  binDict :: Static (Dict (Binary a))  -- todo : conglomerate?
  typDict :: Static (Dict (Typeable a))


-- | Any serialisable value can be considered a 'Closure'
closurePure :: Serializable a => a -> Closure a
closurePure a = closureApp (closureS $ sExtractDecode typDict `staticApp` binDict) $ closureEnc (encode a)
