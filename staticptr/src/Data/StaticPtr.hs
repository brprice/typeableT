-- | Stability: experimental
--
-- Support for static pointers: pointers to values that are known at compile time.

{-# LANGUAGE Safe #-}
{-# LANGUAGE GADTs , FlexibleInstances , ScopedTypeVariables #-}

module Data.StaticPtr (Dict(Dict)
                      ,StaticPtr()
                      ,RemoteTable()
                      ,initRemoteTable
                      ,registerStaticMono
                      ,staticMonoPtr
                      ,Tag(PolyTag,typeableConstraint)
                      ,PolyTblEnt(PolyTblEnt)
                      ,registerStaticPoly
                      ,staticPolyPtrAt
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
                      ) where

import safe Data.Binary(Binary(put,get),Put(),Get(),putWord8,getWord8)
import qualified Data.Map as M (insert,lookup)

import safe Data.TypeableT(TypeRep(),Typeable(typeRep),G1(G1),getR1,G2(G2),getFnR,gcastR)
import safe Data.DynamicT(Dynamic(Dynamic),SDynamic(SDynamic))

import safe Data.StaticPtr.Internals
import safe Data.StaticPtr.SPT

newtype RemoteTable = RT (StaticPtrTable,PolyPtrTable)

initRemoteTable :: RemoteTable
initRemoteTable = RT (initStaticMonoTable,initStaticPolyTable)

-- | Add a monomorphic static pointer to the table
registerStaticMono :: String -> Dynamic -> RemoteTable -> RemoteTable
registerStaticMono name val (RT (SPT spt,ppt)) = RT (SPT $ M.insert name val spt,ppt)

-- | Add a polymorphic static polymorphic to the table
registerStaticPoly :: String -> PolyTblEnt -> RemoteTable -> RemoteTable
registerStaticPoly name val (RT (spt,PPT ppt)) = RT (spt,PPT $ M.insert name val ppt)

-- | Extract a monomorphic 'Static' from the table.
-- This may call 'error', and is only intended to be used in the following pattern
--
-- > rtable = registerStaticMono "true" (toDyn True) initRemoteTable
-- > staticTrue = staticMonoPtr rtable "true"
staticMonoPtr :: forall a . Typeable a => RemoteTable -> String -> Static a
staticMonoPtr rtable n = case lookupSPT rtable n of
                           Just (SDynamic t v) -> case gcastR t (typeRep :: TypeRep a) (staticMono v) of
                                                    Just p -> p
                                                    Nothing -> error $ "bad type on label: " ++ n
                           _ -> error $ "bad label: " ++ n

-- | Extract a polymorphic 'Static' from the table.
-- This may call 'error', and is only intended to be used in the following pattern
--
-- > rtable = registerStaticPoly "id" (PolyTblEnt IdTag (\_ -> id)) initRemoteTable
-- > staticId = staticPolyPtrAt rtable IdTag "id"
--
-- (where IdTag is a suitable type with a suitable 'PolyTag' instance)
staticPolyPtrAt :: forall tag a . Tag tag => RemoteTable -> tag -> String -> Static (Dict (Typeable a)) -> Static (PolyTag tag a)
staticPolyPtrAt rtable tag n typ = case lookupPPT rtable n typ of
                                     Just (SDynamic t v) -> case gcastR t ta v of
                                                              Just p -> p
                                                              Nothing -> error $ "bad type on label: " ++ n
                                     _ -> error $ "bad label: " ++ n
    where ta :: TypeRep (PolyTag tag a)
          ta = (\Dict -> typeRep) $ typeableConstraint tag (deRefStatic typ)

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

lookupSPT :: RemoteTable -> StaticName -> Maybe (SDynamic StaticPtr)
lookupSPT (RT (SPT spt,_)) name = do Dynamic typ ptr <- M.lookup name spt
                                     return $ SDynamic typ (StaticPtr name ptr)

lookupPPT :: RemoteTable -> StaticName -> Static (Dict (Typeable a)) -> Maybe (SDynamic Static) -- Note that a :: *
lookupPPT (RT (_,PPT ppt)) name ta = do PolyTblEnt t f <- M.lookup name ppt
                                        return $ (\Dict -> SDynamic typeRep (PolyPtr name t ta f)) $ typeableConstraint t (deRefStatic ta)

-- We can't provide Binary instances, as need to differ depending on RemoteTable.
-- We force users to provide (orphan) instances

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
getSDynStaticPtr :: RemoteTable -> Get (SDynamic StaticPtr)
getSDynStaticPtr rt = do name <- get
                         maybeToGet "getSDynStaticPtr: lookup failed" $ lookupSPT rt name

-- | Deserialise a 'StaticPtr'. See also 'encode' (default definition in terms of 'getStaticPtr') from the 'Binary' instance of 'StaticPtr'
getStaticPtr :: RemoteTable -> TypeRep a -> Get (StaticPtr a)
getStaticPtr rt t = do SDynamic ta sa <- getSDynStaticPtr rt
                       maybeToGet ("getStaticPtr: dynamic typecheck failed (had "
                                   ++ show t ++ " and " ++ show ta ++ ")")
                         $ gcastR ta t sa

-- | Deserialise a 'Static' to a 'SDynamic' value, without having to know the type.
getSDynStatic :: RemoteTable -> Get (SDynamic Static)
getSDynStatic rt = getWord8 >>= go
  where go 0 = do SDynamic t p <- getSDynStaticPtr rt -- MonoPtr case
                  return $ SDynamic t (MonoPtr p)
        go 1 = do name <- get :: Get StaticName  -- TODO : why need type sig? -- PolyPtr case
                  (SDynamic ttyp typ) <- getSDynStatic rt
                  G1 ttyp' <- maybeToGet "getSDynStatic (poly case): static Dict (Typeable _) arg is not actually a Dict _"
                                       $ getR1 typeRepDict ttyp
                  G1 _ <- maybeToGet "getSDynStatic (poly case): static Dict (Typeable _) arg is not actually a Dict (Typeable _)"
                                   $ getR1 (typeRepTypeable :: TypeRep (Typeable {-:: * -> Constraint-})) ttyp'
                  maybeToGet "getSDynStatic (poly case): lookup failed" $ lookupPPT rt name typ
        go 2 = do SDynamic tf f <- getSDynStatic rt -- StaticApp case
                  SDynamic tx x <- getSDynStatic rt
                  G2 tsrc ttgt <- maybeToGet "getSDynStatic (app case): not a function" $ getFnR tf
                  x' <- maybeToGet "getSDynStatic (app case): function expects different type to argument given"
                        $ gcastR tx tsrc x
                  return $ SDynamic ttgt (StaticApp f x')
        go _ = fail "getSDynStaticPtr: bad encoding"


-- | Deserialise a 'Static'. See also 'encode' (default definition in terms of 'getStatic') from the 'Binary' instance of 'Static'
getStatic :: RemoteTable -> TypeRep a -> Get (Static a)
getStatic rt t = do SDynamic ta sa <- getSDynStatic rt
                    maybeToGet ("getStatic: dynamic typecheck failed (had "
                                ++ show t ++ " and " ++ show ta ++ ")")
                      $ gcastR ta t sa
