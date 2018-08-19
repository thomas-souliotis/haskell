{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE ExistentialQuantification #-}
{-#LANGUAGE InstanceSigs #-}

module Task5 where

import Control.DeepSeq
import Data.IORef
import Control.Monad

-- | Implemented for help
newtype IVar a = IVar (IORef (IVarContents a))

-- | Implemented for help
data IVarContents a = Full a | Empty | Blocked [a -> Trace]

-- | Equality for IVars is physical equality, as with other reference types.
instance Eq (IVar a) where
  (IVar r1) == (IVar r2) = r1 == r2

-- | Implemented for help
instance NFData (IVar a) where
  rnf _ = ()

-- | The Trace data structure required. This structure has many similarities with the GP structure.
-- At first, it also has the Get and Put which are very similar to the Get and Put fields of GP,
-- however now the structure has one more field (Ivar a), and Int is replaced for arbitary a. Of course
-- the principles remain the same for them. Moreover, the Trace structure has some more fields, which of
-- course are not included in the GP structure, but that does not differ them so much, as they may be even
-- implemented in GP if required.
data Trace = forall a . Get (IVar a) (a -> Trace)
           | forall a . Put (IVar a) a Trace
           | forall a . New (IVarContents a) (IVar a -> Trace)
           | Fork Trace Trace
           | Done
           | Yield Trace
           | forall a . LiftIO (IO a) (a -> Trace)

-- | The Par stucture required. This structure is very similar to GP' structure. Moreover,
-- if we inspect it more we will notice that it is almost the same, and it connects Par to Trace
-- the same way that GP' is connected to GP.
newtype Par a = Par {
    runCont :: (a -> Trace) -> Trace
}

-- | Functor instance of Par
instance Functor Par where
    fmap f m = Par $ \c -> runCont m (c . f)

-- | Monad instance of Par
instance Monad Par where
    return = pure
    m >>= k  = Par $ \c -> runCont m $ \a -> runCont (k a) c

-- | Applicative instance of Par
instance Applicative Par where
   (<*>) = ap
   pure a = Par ($ a)

-- | TraceF is similar to Trace only now is ready to become Free.
data TraceF b = forall a . GetF (IVar a) (a -> b)
              | forall a . PutF (IVar a) a b
              | forall a . NewF (IVarContents a) (IVar a -> b)
              | ForkF b b
              | YieldF b
              | forall a . LiftIOF (IO a) (a -> b)

-- | The Free type of TraceF.
type Trace' = Free TraceF


-- | toTraceF takes a Trace and transforms it to Free TraceF.
toTraceF :: Trace -> Free TraceF ()
toTraceF (Get x f)    = Wrap $ GetF x (toTraceF . f)
toTraceF (Put x a b)  = Wrap $ PutF x a $ toTraceF b
toTraceF (New x f)    = Wrap $ NewF x (toTraceF . f)
toTraceF (Fork a b)   = Wrap $ ForkF (toTraceF a) (toTraceF b)
toTraceF (Yield a)    = Wrap $ YieldF $ toTraceF a
toTraceF (LiftIO a f) = Wrap $ LiftIOF a (toTraceF . f)
toTraceF Done         = Return ()

-- | Inverse of toTraceF. The two functions show the isomorphism between the structs.
fromTraceF ::  Free TraceF () -> Trace
fromTraceF (Wrap (GetF x f))    = Get    x (fromTraceF . f)
fromTraceF (Wrap (PutF x a b))  = Put x a (fromTraceF b)
fromTraceF (Wrap (NewF x f))    = New x (fromTraceF . f)
fromTraceF (Wrap (ForkF a b))   = Fork   (fromTraceF a) (fromTraceF b)
fromTraceF (Wrap (YieldF a))    = Yield  (fromTraceF a)
fromTraceF (Wrap (LiftIOF a f)) = LiftIO a (fromTraceF . f)
fromTraceF (Return ())          = Done

-- | The Free struct. (could also be imported with different fields)
data Free f a =
    Return a
  | Wrap (f (Free f a))

-- | Functor instance of Free.
instance Functor f => Functor (Free f) where
  fmap = liftM

-- | Applicative instance of Free.
instance Functor f => Applicative (Free f) where
  pure = return
  (<*>) = ap

-- | Monad instance of Free.
instance Functor f => Monad (Free f) where
  return :: a -> Free f a
  return = Return
  (>>=) :: Free f a -> (a -> Free f b) -> Free f b
  Return a >>= cont = cont a
  Wrap m   >>= cont = Wrap $ fmap (>>= cont) m
