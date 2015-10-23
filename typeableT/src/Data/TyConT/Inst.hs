-- | Stability: experimental
-- Temporary hand-crafted 'TyCon' values for common data types, until GHC gets in-built support

{-# LANGUAGE Safe #-}
{-# LANGUAGE PolyKinds #-}

module Data.TyConT.Inst where

import Data.TyConT.Internals
import Data.TypeableT.Internals(TypeRep())

{- COMPILER MAGIC SHOULD GO HERE & replace the whole module -}
-- Note: the ids are chosen to match the corresponding ids from 'TyCon's in Data.Typeable, at the time of writing, on my machine

tyConBool :: TyCon Bool
tyConBool = TyCon ("ghc-prim","GHC.Types","Bool")

tyConInt :: TyCon Int
tyConInt = TyCon ("ghc-prim","GHC.Types","Int")

tyConMaybe :: TyCon Maybe
tyConMaybe = TyCon ("base","GHC.Base","Maybe")

tyConVoid :: TyCon ()
tyConVoid = TyCon ("ghc-prim","GHC.Tuple","()")

tyConArr :: TyCon (->)
tyConArr = TyCon ("ghc-prim","GHC.Prim","(->)")

tyConList :: TyCon []
tyConList = TyCon ("ghc-prim","GHC.Types","[]")

tyConTypeRep :: TyCon (TypeRep :: k -> *)
tyConTypeRep = TyCon ("typeableT","Data.TypeableT.Internals","TypeRep")
