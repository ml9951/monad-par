{-# LANGUAGE CPP, BangPatterns, NamedFieldPuns #-}

module Control.Monad.Par.Scheds.LinkedDeque (Deque, newDeque, pushWork, popWork, stealWork, tryPopWork)
where

#ifdef PASTMTL2
import Control.TL2.STM
#else
import Control.Concurrent.STM
#endif

data Link a
     = Link
        { prev :: TVar (Link a)
        , next :: TVar (Link a)
        , val  :: a 
        }
     | Head
        { prev :: TVar (Link a)
        , next :: TVar (Link a)
        }

type Deque a = Link a

newDeque :: Int -> STM (Deque a)
newDeque x = do
         prevPtr <- newTVar (error "prevPtr")
         nextPtr <- newTVar (error "nextPtr")
         let n = Head{prev=prevPtr, next=nextPtr}
         writeTVar prevPtr n
         writeTVar nextPtr n
         return n

pushWork :: Deque a -> a -> IO()
pushWork h@Head{prev=hPrev,next=hNext} obj = atomically $ do
         right <- readTVar hNext
         prevPtr <- newTVar h
         nextPtr <- newTVar right
         let newLink = Link{prev=prevPtr, next=nextPtr, val=obj}
         writeTVar hNext newLink
         writeTVar (prev right) newLink

popWork :: Deque a -> STM a
popWork h@Head{prev=hPrev,next=hNext} = do
        right <- readTVar hNext
        case right of
             Head{} -> retry
             Link{next=lNext, prev=lPrev,val} -> do
                  right <- readTVar lNext
                  writeTVar hNext right
                  writeTVar (prev right) h
                  return val

tryPopWork :: Deque a -> STM (Maybe a)
tryPopWork h@Head{prev=hPrev, next=hNext} = do
        right <- readTVar hNext
        case right of
            Head{} -> return Nothing
            Link{next=lNext, prev=lPrev,val} -> do
                  right <- readTVar lNext
                  writeTVar hNext right
                  writeTVar (prev right) h
                  return (Just val)

stealWork :: Deque a -> STM a
stealWork h@Head{prev=hPrev, next=hNext} = do
          left <- readTVar hPrev
          case left of
               Head{} -> retry
               Link{next=lNext, prev=lPrev, val} -> do
                    left <- readTVar lPrev
                    writeTVar hPrev left
                    writeTVar(next left) h
                    return val
