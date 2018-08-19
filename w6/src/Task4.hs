{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE ScopedTypeVariables #-}


module Task4 where

import Control.Monad
import Text.Read           (readMaybe)

-- | The GP structure as implemented in previous week.
data GP a =
    End a
  | Get (Int -> GP a)
  | Put Int (GP a)

-- | The run function as implemented in previous week.
run :: GP a -> IO a
run (End x)   = return x
run (Get f)   = readInt >>= run . f
 where
  readInt :: IO Int
  readInt = do
    putStr "? "
    s <- getLine
    maybe readInt return $ readMaybe s
run (Put n m) = print n >> run m

-- | The add function as implemented in previous week.
add :: GP ()
add =
  Get $ \n ->
  Get $ \m ->
  Put (m + n) $
  End ()

-- | The Monad instance of GP structure as implemented in previous week.
instance Monad GP where
  return = End
  End x >>= f = f x
  Get k >>= f = Get (\ x -> k x >>= f)
  Put x k >>= f = Put x (k >>= f)

-- | The Functor instance of GP structure as implemented in previous week.
instance Functor GP where
  fmap = liftM

-- | The Applicative instance of GP structure as implemented in previous week.
instance Applicative GP where
  pure  = return
  (<*>) = ap

-- | A get function for GP.
get :: GP Int
get = Get End

-- | A put function for GP.
put :: Int -> GP ()
put x = Put x (End ())

-- | The simulate function as implemented in previous week.
simulate :: GP a -> [Int] -> a
simulate (End x)   _        = x
simulate (Put _ k) is       = simulate k is
simulate (Get k)   (i : is) = simulate (k i) is
simulate _         _        = error "inaprorpriate case"

-- | 4.1, askmany "asks" that many questions(meaning that many get), and the final result int is the sum of all the input
-- int that were given.
askMany :: Int -> GP Int
askMany 0 = return 0
askMany n = askMany (n - 1) >>= \ n -> get >>= \ x -> return (n + x)

-- | testaskMany crashes if we try that. This happens due to the quadratic nature of the final result,
-- since we need n repetitions of askMany due to the simulate part, and every repetition requires n computations.
-- >>>simulate (askMany 1000) (repeat 1)
-- 1000
testaskMany :: Int
testaskMany = simulate (askMany 100000) (repeat 1)

-- | 4.4.2, GP' is isomorphic to GP, with some goods.
newtype GP' a = GP' { unGP' :: forall b . (a -> GP b) -> GP b }

-- | Takes a GP and transforms it to GP'
fromGP :: GP a -> GP' a
fromGP u = GP' $ (>>=) u

-- | Takes a GP' and transformsit to GP
toGP   :: GP' a -> GP a
toGP  = ($ return) . unGP'

-- | Sample for test the from and to isomorphism
testFromTo :: IO ()
testFromTo = run $ toGP $ fromGP add

-- | 4.3
-- Same as get but works on GP' instead
get' :: GP' Int
get' = fromGP get

-- | Same as put but works on GP' instead
put' :: Int -> GP' ()
put' n = fromGP $ put n

-- | Same as simulate but works on GP' instead
simulate' :: GP' a -> [Int] -> a
simulate' gp = simulate (toGP gp)

-- | 4.4
-- Functor instance of GP'
instance Functor GP' where
  fmap = liftM

-- | Applicative instance of GP'
instance Applicative GP' where
  pure  = return
  (<*>) = ap

-- | Monad instance of GP'
instance Monad GP' where
  return a = GP' $ \k -> k a
  a >>= b =
    let
      f1 = flip (unGP' . b)
      f2 = unGP' a
    in
      GP' $ \k -> f2 (f1 k)

-- | 4.5, askMany' is exactly same as askMany, only now we have it in terms of GP' instead of GP2
askMany' :: Int -> GP' Int
askMany' 0 = return 0
askMany' n = askMany' (n - 1) >>= \ n -> get' >>= \ x -> return (n + x)

-- | Same as testaskMany' but with GP' instead.
-- >>>testaskMany'
-- 100000
testaskMany' :: Int
testaskMany' = simulate' (askMany' 100000) (repeat 1)
