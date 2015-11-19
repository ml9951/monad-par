{-# LANGUAGE RankNTypes, NamedFieldPuns, BangPatterns,
             ExistentialQuantification, CPP, DoAndIfThenElse #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

-- | This module exposes the internals of the @Par@ monad so that you
-- can build your own scheduler or other extensions.  Do not use this
-- module for purposes other than extending the @Par@ monad with new
-- functionality.

module Control.Monad.Par.Scheds.TraceInternal (
   Trace(..), Sched(..), Par(..),
   IVar(..), IVarContents(..),
   sched,
   runPar, runParIO, runParAsync,
   -- runParAsyncHelper,
   new, newFull, newFull_, get, put_, put,
   pollIVar, yield,
 ) where


import Control.Monad as M hiding (mapM, sequence, join)
import Prelude hiding (mapM, sequence, head,tail)
import Data.IORef
import System.IO.Unsafe
import Control.Concurrent hiding (yield)
import GHC.Conc (numCapabilities)
import Control.DeepSeq

#ifdef LINKED_DEQUE
import qualified Control.Monad.Par.Scheds.LinkedDeque as D
#else
import qualified Control.Monad.Par.Scheds.ResizableDeque as D
#endif

-- import Text.Printf

#ifdef PASTMTL2
import Control.TL2.STM
#else
import Control.Concurrent.STM
#endif

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

#if __GLASGOW_HASKELL__ <= 700
import GHC.Conc (forkOnIO)
forkOn = forkOnIO
#endif


-- ---------------------------------------------------------------------------

data Trace = forall a . Get (IVar a) (a -> Trace)
           | forall a . Put (IVar a) a Trace
           | forall a . New (IVarContents a) (IVar a -> Trace)
           | Fork Trace Trace
           | Done
           | Die
           | Yield Trace
           | forall a . LiftIO (IO a) (a -> Trace)

-- | The main scheduler loop.
sched :: Bool -> Sched -> Trace -> IO ()
sched _doSync queue t = loop t
 where
  loop t = case t of
    New a f -> do
      r <- newIORef a
      loop (f (IVar r))
    Get (IVar v) c -> do
      e <- readIORef v
      case e of
         Full a -> loop (c a)
         _other -> do
           r <- atomicModifyIORef v $ \e -> case e of
                        Empty    -> (Blocked [c], reschedule queue)
                        Full a   -> (Full a,      loop (c a))
                        Blocked cs -> (Blocked (c:cs), reschedule queue)
           r
    Put (IVar v) a t  -> do
      cs <- atomicModifyIORef v $ \e -> case e of
               Empty    -> (Full a, [])
               Full _   -> error "multiple put"
               Blocked cs -> (Full a, cs)
      mapM_ (D.pushWork (workpool queue) . ($a)) cs
      loop t
    Fork child parent -> do
         D.pushWork (workpool queue) child
         loop parent
    Done ->
         if _doSync
         then reschedule queue
-- We could fork an extra thread here to keep numCapabilities workers
-- even when the main thread returns to the runPar caller...
         else do putStrLn " [par] Forking replacement thread..\n"
                 forkIO (reschedule queue); return ()
-- But even if we don't we are not orphaning any work in this
-- threads work-queue because it can be stolen by other threads.
--       else return ()
    Yield parent -> do
        D.pushWork (workpool queue) parent
        reschedule queue
    LiftIO io c -> do
        r <- io
        loop (c r)
    Die -> return()

incTVar :: TVar Int -> Int -> STM ()
incTVar t i = do
        x <- readTVar t
        writeTVar t (x+i)

-- | Process the next item on the work queue or, failing that, go into
--   work-stealing mode.  We keep track of how many threads are searching
--   for work.  If N-1 threads are trying to steal, and I also am trying
--   to steal, then it must be the case that everyone is asleep in a retry.
reschedule :: Sched -> IO ()
reschedule queue@Sched{ workpool, shutdown, lookingForWork } = do
           mTask <- atomically (D.tryPopWork workpool)
           case mTask of
                Just t -> sched True queue t
                Nothing -> do
                        continue <- atomically $ do
                                   n <- readTVar lookingForWork
                                   if n == numCapabilities - 1 --my deque is empty and everyone else is looking for work
                                   then writeTVar shutdown True >> return False  --no one has anything left so we are odne
                                   else writeTVar lookingForWork (n+1) >> return True
                        if continue
                        then do t <- atomically(steal queue); atomically (incTVar lookingForWork (-1)); sched True queue t
                        else return()
                        
-- RRN: Note -- NOT doing random work stealing breaks the traditional
-- Cilk time/space bounds if one is running strictly nested (series
-- parallel) programs.

-- | Attempt to steal work or, failing that, give up and go idle.
steal :: Sched -> STM Trace
steal Sched{ scheds, no=my_no, shutdown } = do
  -- printf "cpu %d stealing\n" my_no
  go scheds
  where
    go [] = do
       done <- readTVar shutdown
       if done
       then return Die
       else retry
    go (x:xs)
      | no x == my_no = go xs
      | otherwise     = D.stealWork (workpool x) `orElse` go xs

data Sched = Sched
    { no       :: {-# UNPACK #-} !Int,
      workpool :: D.Deque Trace,
      shutdown :: TVar Bool,
      lookingForWork :: TVar Int,
      scheds   :: [Sched] -- Global list of all per-thread workers.
    }
--  deriving Show

newtype Par a = Par {
    runCont :: (a -> Trace) -> Trace
}

instance Functor Par where
    fmap f m = Par $ \c -> runCont m (c . f)

instance Monad Par where
    return a = Par ($ a)
    m >>= k  = Par $ \c -> runCont m $ \a -> runCont (k a) c

instance Applicative Par where
   (<*>) = ap
   pure  = return

newtype IVar a = IVar (IORef (IVarContents a))
-- data IVar a = IVar (IORef (IVarContents a))

-- | Equality for IVars is physical equality, as with other reference types.
instance Eq (IVar a) where
  (IVar r1) == (IVar r2) = r1 == r2

-- Forcing evaluation of a IVar is fruitless.
instance NFData (IVar a) where
  rnf _ = ()


-- From outside the Par computation we can peek.  But this is nondeterministic.
pollIVar :: IVar a -> IO (Maybe a)
pollIVar (IVar ref) =
  do contents <- readIORef ref
     case contents of
       Full x -> return (Just x)
       _      -> return (Nothing)


data IVarContents a = Full a | Empty | Blocked [a -> Trace]


{-# INLINE runPar_internal #-}
runPar_internal :: Bool -> Par a -> IO a
runPar_internal _doSync x = do
   workpools <- replicateM numCapabilities $ atomically $ D.newDeque 72 --Is 72 reasonable?
   (shutdown, lookingForWork) <- atomically $ do x <- newTVar False; y <- newTVar 0; return(x, y)
   let states = [ Sched { no=x, workpool=wp, shutdown, lookingForWork, scheds=states }
                | (x,wp) <- zip [0..] workpools ]

#if __GLASGOW_HASKELL__ >= 701 /* 20110301 */
    --
    -- We create a thread on each CPU with forkOn.  The CPU on which
    -- the current thread is running will host the main thread; the
    -- other CPUs will host worker threads.
    --
    -- Note: GHC 7.1.20110301 is required for this to work, because that
    -- is when threadCapability was added.
    --
   (main_cpu, _) <- threadCapability =<< myThreadId
#else
    --
    -- Lacking threadCapability, we always pick CPU #0 to run the main
    -- thread.  If the current thread is not running on CPU #0, this
    -- will require some data to be shipped over the memory bus, and
    -- hence will be slightly slower than the version above.
    --
   let main_cpu = 0
#endif

   m <- newEmptyMVar
   forM_ (zip [0..] states) $ \(cpu,state) ->
        forkOn cpu $
          if (cpu /= main_cpu)
             then reschedule state
             else do
                  rref <- newIORef Empty
                  sched _doSync state $ runCont (x >>= put_ (IVar rref)) (const Done)
                  readIORef rref >>= putMVar m

   r <- takeMVar m
   case r of
     Full a -> return a
     _ -> error "no result"


runPar :: Par a -> a
runPar = unsafePerformIO . runPar_internal True

-- | A version that avoids an internal `unsafePerformIO` for calling
--   contexts that are already in the `IO` monad.
runParIO :: Par a -> IO a
runParIO = runPar_internal True

-- | An asynchronous version in which the main thread of control in a
-- Par computation can return while forked computations still run in
-- the background.
runParAsync :: Par a -> a
runParAsync = unsafePerformIO . runPar_internal False

-- -----------------------------------------------------------------------------

-- | Creates a new @IVar@
new :: Par (IVar a)
new  = Par $ New Empty

-- | Creates a new @IVar@ that contains a value
newFull :: NFData a => a -> Par (IVar a)
newFull x = deepseq x (Par $ New (Full x))

-- | Creates a new @IVar@ that contains a value (head-strict only)
newFull_ :: a -> Par (IVar a)
newFull_ !x = Par $ New (Full x)

-- | Read the value in an @IVar@.  The 'get' operation can only return when the
-- value has been written by a prior or parallel @put@ to the same
-- @IVar@.
get :: IVar a -> Par a
get v = Par $ \c -> Get v c

-- | Like 'put', but only head-strict rather than fully-strict.
put_ :: IVar a -> a -> Par ()
put_ v !a = Par $ \c -> Put v a (c ())

-- | Put a value into an @IVar@.  Multiple 'put's to the same @IVar@
-- are not allowed, and result in a runtime error.
--
-- 'put' fully evaluates its argument, which therefore must be an
-- instance of 'NFData'.  The idea is that this forces the work to
-- happen when we expect it, rather than being passed to the consumer
-- of the @IVar@ and performed later, which often results in less
-- parallelism than expected.
--
-- Sometimes partial strictness is more appropriate: see 'put_'.
--
put :: NFData a => IVar a -> a -> Par ()
put v a = deepseq a (Par $ \c -> Put v a (c ()))

-- | Allows other parallel computations to progress.  (should not be
-- necessary in most cases).
yield :: Par ()
yield = Par $ \c -> Yield (c ())
