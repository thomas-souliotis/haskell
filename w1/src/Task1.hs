{-# Language DeriveFoldable #-}
--Task 2--

module Task1 where

import qualified Data.Map as M
import System.IO
import Prelude hiding (empty)



-- | a simple elementAt function which finds
-- >>> elementAt  [1..10000000000000000000000000] 3 = 3
elementAt :: [a] -> Int -> Maybe a
elementAt [] _ = Nothing
elementAt (x:xs) ind =
  if ind <= 0 then Nothing
              else Just $ elementAt' (x:xs) ind

-- | helper function for elementAt
elementAt' :: [a] -> Int -> a
elementAt' []     _   = undefined
elementAt' (x:xs) ind =
  if ind ==1 then x
             else elementAt' xs (ind -1)

-- | A function that clears a list from duplicates
-- >>> compress [1,2,3,4,4,4,3,3,3,1] = [1,2,3,4]
compress :: (Eq a) =>[a] -> [a]
compress [] = []
compress (x:xs) = (x: compress (filter (p x) xs))
  where
    p :: (Eq a) => a -> a -> Bool
    p a y  = a/=y


-- | Function that computes the factorial of a number
-- >>> (toList . fromList) [1,2,3] == id [1,2,3]
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

-- | permutations of a list with all possible permutations
--- >>> permutations [1,2,3] = [[1,2,3],[1,3,2],[2,3,1],[2,1,3],[3,1,2],[3,2,1]]
permutations :: [a] -> [[a]]
permutations []     = [[]]
permutations (x:xs) =
  let
    res = (map (x:) (permutations xs))
  in
    take (factorial (length (x:xs))) (res  ++ (permutations (xs++[x])))


--Task 3--
-- | mergersot function
-- >>>  mergesort [3,4,5,1,2] = [1,2,3,4,5]
mergesort :: (Ord a) => [a] -> [a]
mergesort []     = []
mergesort (x:[]) = [x]
mergesort xs     = merge (mergesort xs') (mergesort ys')
  where
   (xs',ys') = splitAt (length xs `div` 2) xs

-- | the merge part of mergesort
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) =
  if x <= y then (x: merge xs (y:ys))
            else (y: merge (x:xs) ys)




-- | a tree data struct
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Eq, Show, Foldable)

-- | splitleft function of the above tree struct
-- >>> splitleft example1 = (1,Just (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))
splitleft :: Tree a -> (a, Maybe (Tree a))
splitleft (Leaf a)          = (a, Nothing)
splitleft (Node (Leaf a) r) = (a, Just r)
splitleft (Node l r)        = splitleft' l r

-- | helper function for splitleft
splitleft' :: Tree a -> Tree a -> (a, Maybe (Tree a))
splitleft' (Leaf a)     r = (a, Just r)
splitleft' (Node l' r') r = splitleft' l' (Node r' r)


-- | examples of tree data structs
example1 :: Tree Int
example1 = (Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4)))
example2 :: Tree Int
example2 = (Node (Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))) (Node (Leaf 5) (Leaf 6)))


-- | a trie struct
data Trie a = Fork (Maybe a) (M.Map Char (Trie a)) deriving (Eq, Show, Foldable)

--instance Foldable Trie where
  --foldr = foldrBinTree

--instance Functor Trie where
  --fmap = mapBinTree

-- | produces an empty trie
-- >>> empty' = Fork Nothing (fromList [])
empty'  :: Trie a
empty' = Fork Nothing M.empty

-- | a null test of trie
null' :: Trie a -> Bool
null' empty' = True
null' _      = False

sample_tree2 :: Trie Int
sample_tree2 = Fork Nothing  (M.fromList [('r', Fork (Just 2) M.empty)])

-- | looks up the value associated with the key
-- >>> lookup' "aa" sample_tree2 =Nothing
-- >>> lookup' "r" sample_tree2  =Just 2
lookup' :: String -> Trie a -> Maybe a
lookup' [] _ = Nothing
lookup' (x:xs) (Fork a b) =
  case xs of [] -> lookupaux x (Fork a b)
             _  -> case lookupaux2 x (Fork a b) of Nothing -> Nothing
                                                   Just t  -> lookup' xs t

lookupaux :: Char -> Trie a -> Maybe a
lookupaux key (Fork _ b) =
  case M.lookup key b of Nothing         -> Nothing
                         Just (Fork a _) -> a

lookupaux2 :: Char -> Trie t -> Maybe (Trie t)
lookupaux2 key (Fork _ b) =  M.lookup key b

-- | Another data structure
data GP a =
    End a
  | Get (Int -> GP a)
  | Put Int (GP a)

-- | An echo function based on the above data structure
echo :: GP a
echo = Get $ \n -> Put n echo

-- | the same but with Input and output from keyoboard
run :: GP a -> IO a
run f =
   case f of Get f' -> do
                         a<- readLn
                         run (f' a)
             End f' -> return f'
             Put n f' -> do
                           putStrLn $ show n
                           run f'


