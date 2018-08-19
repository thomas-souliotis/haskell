{-# LANGUAGE BangPatterns #-}

module Task2  where

import Control.Monad
import Control.Applicative


data Delayed a = Now a | Later (Delayed a) deriving (Eq, Show )

-- | An unsafe way trying to get the a from a Delayed a
unsafeRunDelayed :: Delayed a -> a
unsafeRunDelayed (Now x)   = x
unsafeRunDelayed (Later d) = unsafeRunDelayed d

-- | A loop function to create an infinite Delayed, with infinite Later's
loop :: Delayed a
loop = Later loop

-- | A function to create the factorial of a given Int in terms of a Delayed Int.
factorial :: Int -> Delayed Int
factorial = go 1
  where
    go !acc n
      | n <= 0    = Now acc
      | otherwise = Later (go (n * acc) (n - 1))

-- | 5.2.1, the required runDelayed function that returns a Just result if it has up to n many Later's
-- and a Nothing result if not.
runDelayed :: Int -> Delayed a -> Maybe a
runDelayed 0 (Later _) = Nothing
runDelayed _ (Now x) = Just x
runDelayed n (Later d) = runDelayed (n-1) d


-- | 5.2.2 The functor, Alternative and Monad instance of Delayed struct.
instance Functor Delayed where
  fmap = liftM

instance Applicative Delayed where
  pure = return
  (<*>) = ap

instance Monad Delayed where
  return = Now
  Now x >>= cont = cont x
  Later x >>= cont = Later $ x >>= cont

-- | 5.2.3
tick :: Delayed ()
tick = Later (Now ())

-- | psum creates a Delayed with n Later's where n = length of the input list, and then Now sum
-- where sum = sum elements input list. More preciasely the number of Laters is implemented with the tick,
-- and we  fmap the sum to the result of the right function, with the list input as input.
psum :: [Int] -> Delayed Int
psum xs = sum <$> mapM (\ x -> tick >> return x) xs


-- | 5.2.4

-- | The free data structure. Could also be imported
data Free f a =
  Return a
  | Wrap (f (Free f a))

-- | The delayedF instance for the free transformations
data DelayedF a = NowF a | LaterF (DelayedF a) deriving (Show)

-- | The functor instance of DelayedF
instance Functor DelayedF where
  fmap f (NowF x)   = NowF $ f x
  fmap f (LaterF x) = LaterF $ fmap f x

-- | A function from Delayed a to Free Delayed a. Useful for the isomorfism.
fromDelayed :: Delayed a -> Free DelayedF a
fromDelayed (Now x)   = Return x
fromDelayed (Later x) =
  case fromDelayed x of
    Wrap sth  -> Wrap $ LaterF sth
    Return a  -> Wrap $ LaterF $ NowF $ Return a

-- | The reverse of fromDelayed
toDelayed :: Free DelayedF a -> Delayed a
toDelayed (Return x) = Now x
toDelayed (Wrap x)   =
  case x of
    NowF a   -> toDelayed a
    LaterF a -> Later $ toDelayed $ Wrap a


-- | 5.2.5

-- | The Alternative instance of Delayed.
instance Alternative Delayed where
  empty = loop
  (<|>) = merge

-- | A function that given two Delay structs merges them into one.
merge :: Delayed a -> Delayed a -> Delayed a
merge (Now x) _           = Now x
merge _ (Now x)           = Now x
merge (Later p) (Later q) = Later (merge p q)


-- | The requested function that that performs psum on every of the integer lists
--  and returns the result that can be obtained with as few delays as possible.
firstSum :: [[Int]] -> Delayed Int
firstSum xs =
  let
    xs' = map psum xs
  in
    merge' xs'

-- | A helper function
merge' :: [Delayed Int] -> Delayed Int
merge' = foldr merge loop

-- | 5.2.6

-- | biasedMerge not actually required as in the final solution it is implemented internally
biasedMerge :: (Eq a) => Delayed a -> Delayed a -> Delayed a
biasedMerge (Now x) _           = Now x
biasedMerge _ (Now x)           = Now x
biasedMerge (Later a) (Later b) =
  let
    res1 = check1 a 10
    res2 = check1 b 10
    result = res1-res2
  in
    if (result == 0) && (res1 == 10000)  then Later a
                                         else Later $ merge a b


-- | Checks if it has up to 1000 Later and if show then returns 10000 else it returns the n
check1 :: Delayed a -> Int -> Int
check1 a n =
  if n > 1000 then n
              else case runDelayed n a of Nothing -> check1 a (10*n)
                                          Just _  -> n

-- | same as firstsum but calls biasedMerge'
biasedFirstSum :: [[Int]] -> Delayed Int
biasedFirstSum xs =
  let
    xs' = map psum xs
  in
    biasedMerge' xs'

-- | If something has more than 1000 laters then checks the next arguement. The merge x arguement is
-- | actually not required thus.
biasedMerge' :: [Delayed Int] -> Delayed Int
biasedMerge' [] = error "emty list"
biasedMerge' [x] = x
biasedMerge' (x:y:ys) =
  let
    res1 = check1 x 1
  in
    if res1 == 10000 then biasedMerge' (y:ys)
                     else x


-- sample for testing, uncomment if you want to use them
{-
task2ex :: Delayed ()
task2ex = Now ()

task2ex1 :: Delayed ()
task2ex1 = Later (Now ())

task2ex2 :: Delayed ()
task2ex2 = Later (Later (Now ()))

task2ex3 :: Delayed ()
task2ex3 = Later (Later (Later (Now ())))
-}