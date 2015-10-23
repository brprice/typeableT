{-# LANGUAGE GADTs , KindSignatures , ScopedTypeVariables #-}

-- | A mockup of a simple process framework using 'Control.DistributedClosure'
-- It only handles local threads though.
--
-- Also, a function that really requires our polymorphism support: 'purifyAllListMaybeBoolInt'
-- which takes closures and makes them of the form @closurePure _@
-- (if they are a closure of some combination of Int, Bool, [] and Maybe, and leaves others alone)
-- Its little brother is 'purify', which only works for Int and Bool themselves.

import Data.Type.Equality

import Data.TypeableT
import Data.DynamicT
import Data.StaticPtr
import Control.DistributedClosure

import Control.Concurrent
import Control.Monad.RWS
import Data.Binary(Binary(),decode,encode)
import Data.ByteString.Lazy(ByteString)

import Control.Applicative((<|>))
import Data.Binary.Put(runPut)


type ProcessId = Int

type MessageQ = Chan ByteString

-- getMVar reg >>= return.(!!n) is the message queue for thread n
data LocalParams = LP {pid :: ProcessId, reg :: MVar [MessageQ]}

type Process a = RWST LocalParams () MessageQ IO a

getSelfId :: Process ProcessId
getSelfId = pid <$> ask

getMessage :: Process (ByteString)
getMessage = do q <- get
                liftIO $ readChan q

sendMessage :: Binary a => ProcessId -> a -> Process()
sendMessage n m = do lps <- ask
                     qs <- liftIO $ readMVar $ reg lps
                     liftIO $ writeChan (qs!!n) $ encode m

runProc :: MVar Int -> MVar [MessageQ] -> Process a -> IO a
runProc t r p = do n <- takeMVar t
                   qs <- takeMVar r
                   c <- newChan
                   putMVar t $ n+1
                   putMVar r $ qs++[c]
                   (a,mQ,()) <- runRWST p (LP n r) c
                   return a

main = do tid <- newEmptyMVar
          reg <- newEmptyMVar
          putMVar tid 0
          putMVar reg []
          forkIO $ runProc tid reg $ server tid reg
          done <- newEmptyMVar
          forkIO $ runProc tid reg $ client 0 done -- pid 0 is the server
          takeMVar  done


server :: MVar Int -> MVar [MessageQ] -> Process ()
server tid reg = forever $ do m <- getMessage
                              let id :: ProcessId = decode m
                              lift $ forkIO $ runProc tid reg $ worker id

--Works, but only does Int & Bool, below does all combinations of Int, Bool, [], Maybb
workerBasic :: ProcessId -> Process()
workerBasic ret = do id <- getSelfId
                     sendMessage ret id
                     forever $ do m <- getMessage
                                  let c = decode m
                                  case purify purIntBool c
                                    of Just (SDynamic t (PR Dict a)) -> sendMessage ret a
                                       Nothing -> sendMessage ret m

worker :: ProcessId -> Process()
worker ret = do id <- getSelfId
                sendMessage ret id
                forever $ do m <- getMessage
                             case decode m
                               of SDynamic ta ca -> sendMessage ret $ SDynamic ta $ purifyAllListMaybeBoolInt ta ca

client :: ProcessId -> MVar () -> Process()
client srv done = do id <- getSelfId
                     sendMessage srv id
                     m <- getMessage
                     let wrkr = decode m
                     mapM_ (reqPrnt wrkr) requests
                     liftIO $ putMVar done ()
  where reqPrnt wrkr rq = do liftIO $ putStrLn $ "sent:     " ++ show rq --"should get " ++ (show $ unclosure rq)
                             sendMessage wrkr rq
                             res <- getMessage
                             liftIO $ putStrLn $ "recieved: " ++ show (decode res :: Closure Int) --"actually got " ++ (show $ unclosure (decode res :: Closure Int))
        -- We only test Int, but it works for [Maybe Bool] etc also
        requests :: [Closure Int]
        requests = [closurePure 0
                   ,closureApp (closureSP staticSucc) $ closurePure 0
                   ,closureApp (closureApp (closureSP staticAdd) (closurePure 1))
                               (closureApp (closureSP staticSucc) (closurePure 0))
                   ,closureApp (closureSP staticPredBS) (closureEnc $ encode (4::Int))
                   ]

--Works, but only does Int & Bool, 'purifyAllListMaybeBoolInt' below does all combinations of Int, Bool, [], Maybe
newtype DS a = DS (Dict (Serializable a))

purIntBool :: [SDynamic DS]
purIntBool = [toSDyn $ DS (Dict :: Dict (Serializable Int))
             ,toSDyn $ DS (Dict :: Dict (Serializable Bool))
             ]

data PurRet a = PR (Dict (Serializable a)) (Closure a)
purify :: [SDynamic DS] -> SDynamic Closure -> Maybe (SDynamic PurRet)
purify ss (SDynamic (tc :: TypeRep a) c) = go ss
  where go :: [SDynamic DS] -> Maybe (SDynamic PurRet)
        go [] = Nothing
        go (SDynamic ts s : ss) = case eqRRHom tc ts
                             of Nothing -> go ss
                                Just Refl -> case s
                                             of DS Dict -> Just $ SDynamic tc $ PR Dict $ closurePure $ unclosure c


-- Easily extendible to more types of kind *, harder for higher kinds
-- (as in, trivial, but can't just pass in something like [Serializable a => TypeRep a] & map over it,
-- have to hard-code each case
purifyAllListMaybeBoolInt :: TypeRep a -> Closure a -> Closure a
purifyAllListMaybeBoolInt ta ca = maybe ca (\Dict -> closurePure $ unclosure ca) $ getSerListMaybeBoolInt ta

getSerListMaybeBoolInt :: TypeRep a -> Maybe (Dict (Serializable a))
getSerListMaybeBoolInt t = get0 typeRepBool t
                       <|> get0 typeRepInt t
--                       <|> get1 typeRepList t
--                       <|> get1 typeRepMaybe t
                       <|> getList t
                       <|> getMaybe t
  where get0 :: forall a b . Serializable a => TypeRep a -> TypeRep b -> Maybe (Dict (Serializable b))
        get0 t t' = do Refl <- eqRRHom t t'
                       return (Dict :: Dict (Serializable a))
        getList :: TypeRep a -> Maybe (Dict (Serializable a))
        getList t = do G1 t' <- getR1 typeRepList t
                       Dict :: Dict (Serializable a') <- getSerListMaybeBoolInt t'
                       return (Dict :: Dict (Serializable [a']))
        getMaybe :: TypeRep a -> Maybe (Dict (Serializable a))
        getMaybe t = do G1 t' <- getR1 typeRepMaybe t
                        Dict :: Dict (Serializable a') <- getSerListMaybeBoolInt t'
                        return (Dict :: Dict (Serializable (Maybe a')))
       -- we can't write the constraint I want, unfortunately
       -- have to do List & Maybe seperately
{-
        get1 :: forall (a :: * -> *) b . {-(forall c . Serializable c => Serializable (a c)) =>-} TypeRep a -> TypeRep b -> Maybe (Dict (Serializable b))
        get1 t t' = do G1 t' <- getR1 t t'
                       Dict :: Dict (Serializable b') <- getSerListMaybeBoolInt t'
                       return _ --(Dict :: Dict (Serializable (a b')))
-}
