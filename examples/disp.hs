{-# LANGUAGE GADTs #-}

import Data.TypeableT
import Data.Type.Equality
import Data.Char
import Data.Maybe

typeRepChar :: TypeRep Char
typeRepChar = unsafeTemporaryMkTypeRep ("ghc-prim","GHC.Types","Char")

instance Typeable Char where
  typeRep = typeRepChar

-- | A contrived example of using 'Typeable'.
-- Given a @c :: Char@, return @show c@
-- given a string, convert it to upper-case
-- and given anything else, return "unknown"
--
-- >>> disp 'a' = "'a'"
-- >>> disp "Hello, World!" = "HELLO, WORLD!"
-- >>> disp False = "unknown"
disp :: Typeable a => a -> String
disp a = head $ catMaybes [dispChar a, dispString a , Just "unknown"]
  where dispChar :: Typeable a => a -> Maybe String
        dispChar a = do c <- cast a :: Maybe Char
                        return $ show c

        dispString :: Typeable a => a -> Maybe String
        dispString a = do G1 ta' <- getR1 typeRepList $ typeOf a
                          Refl <- eqRRHom typeRepChar ta'
            -- Pattern matching on Refl brings (a ~ [Char]) into scope
                          return $ map toUpper a
