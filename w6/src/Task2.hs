{-#LANGUAGE RankNTypes #-}

module Task2 where

import Prelude hiding (replicate, filter)

build :: (forall r . r -> (a -> r -> r) -> r) -> [a]
build builder = builder [] (:)

-- | same as Prelude.replcate
-- >>>replicate 3 1
-- [1,1,1]
replicate :: Int -> a -> [a]
replicate n x =
  build (\ nil cons ->
    let
      go n =
        if n <= 0
          then nil
          else cons x (go (n - 1))
    in
      go n)


-- | 2.1, fromTo creates a list that given 2 int (a,b), the list contains all the elements between them
-- Of course if a<b the result is the empty list.
-- >>> fromTo 1 5
-- [1,2,3,4,5]
fromTo :: Int -> Int -> [Int]
fromTo a b =
  if a > b then []
           else
            build (\ nil cons ->
              let
                go a' b' =
                  if a' > b' then nil
                             else cons a' (go (a'+1) b')
              in
                go a b)

-- | 2.2 A simple counterexample
-- A helper function instead of writing flip seq.
helpSeq :: b -> a -> b
helpSeq = flip seq

-- | A simple function that gives a different result from the below function.
functionSeq :: Int
functionSeq = seq undefined 0

-- | The function that should have had the same result with the above based on the definition of the example.
-- However the one gives an undefined error, and the other gives 0 as a result. Reason is due to the nature of
-- build fusion with foldr.
functionSeq' :: Int
functionSeq' = foldr undefined 0 (build helpSeq)

-- | 2.3 filter here is the same as Prelude.filter
-- >>>filter (>3) [1..5]
-- [4,5]
filter :: (a -> Bool) -> [a] -> [a]
filter p xs =
  build (\ nil cons ->
    let
      f x = if p x then cons x else id
    in
      foldr f nil xs)