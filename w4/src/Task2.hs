{-# LANGUAGE DeriveGeneric #-}

module Task2  where

import Prelude hiding (reverse)
import GHC.Generics

data Perfect a = Z a | S (Perfect (a, a)) deriving (Show,Generic)

-- | '4.2.1' - creates the reverse of a Perfect a
reverse :: Perfect a -> Perfect a
reverse x = reverse' x id

reverse' :: Perfect a -> (a -> a) -> Perfect a
reverse' x f =
  case x of Z a -> Z $ f a
            S a -> S $ reverse' a (f' f)
              where
                f' :: (a -> a) -> (a , a) -> (a , a)
                f' f'' (a1 , a2) =  (f'' a2, f'' a1)

-- | '4.2.2'

-- | returns the index of a Perfect a
index :: Perfect a -> Int -> Maybe a
index x i = if i > j-1 || i <0 then Nothing
                               else Just (index' x (toBin i (toBinZ j)) id)
                                 where
                                 j = findmaxInd x

-- | aux of index
index' :: Perfect a -> [Int] -> (a -> b) ->  b
index' (Z a) _  f = f a
index' (S a) xs f = index' a xs' (f' f)
  where
    x   = head xs
    xs' = tail xs
    f' f'' (a1,a2) = if x==0 then f'' a1
                             else f'' a2

-- | finds max index possible given a Perfect a - O(log n) complexity
findmaxInd :: Perfect a -> Int
findmaxInd = findmaxInd' choose

-- | used instead of id
choose :: a -> Int
choose _ = 1

-- | aux of findmaxInd
findmaxInd' :: (a -> Int) -> Perfect a -> Int
findmaxInd' k (Z n) = k n
findmaxInd' k (S p) = findmaxInd' newk p
  where
    newk (a1, _) = 2* k a1

-- | log2 and log2' are functions that given an Int number it gives the Int log2 of this number
log2 :: Int -> Int
log2 x = log2' $ fromIntegral x

log2' :: Double -> Int
log2' x = ceiling $ logBase 2 x

-- | toBinZ creates the binary list of a given int with all elements zero
toBinZ :: Int -> [Int]
toBinZ 0 = [0]
toBinZ n = 0:toBinZ (n `div` 2)

-- | toBin creates the binary list of 0,1 of a given int and appends in the end some more zeros
-- | so as to be able to express i as a bin number in terms of j
toBin :: Int -> [Int] -> [Int]
toBin 0 xs = xs
toBin i xs =
  if i `mod` 2 ==1 then 1:toBin (i `div` 2) (tail xs)
                   else 0:toBin (i `div` 2) (tail xs)


-- | 4.2.3 - builds a Perfect a, from [a], with n being the height of Perfect a

build :: Int -> [a] -> Perfect a
build n xs =
  if length xs < 2^n then error "list not big enough"
                     else insertInPerfect per xs 0 j n
                       where
                         per = build' n xs head
                         j = findmaxInd per



-- | creates a Perfect a with all elements the first element of the list

build' ::  Int -> [a] -> ([a] -> b ) -> Perfect b
build' 0 xs f = Z $ f xs
build' n xs f = S $ build' (n-1) xs f'
  where
    f' _ = (f xs, f xs )

-- | creates the final result with inserting one by one all the elements of the remaining list
insertInPerfect :: Perfect a -> [a] -> Int -> Int -> Int -> Perfect a
insertInPerfect per xs i j n =
  if i==2^n then per
            else insertInPerfect (insert' per (head xs) (head l +1) l id) (tail xs) (i+1) j n
              where
                l = toBin i (toBinZ j)



-- | the function that inserts the element x at the Perfect a in the right index
insert' :: Perfect a -> a -> Int -> [Int] -> (a -> a) -> Perfect a
insert' (Z u) _ _ _ f = Z $ f u
insert' (S a) x i l f = S $ insert' a (x,x) 0 l' f'
  where
    ind = head l
    l' =  tail l
    --f' :: (a -> a) -> (a , a) -> (a , a)
    f' (a1,a2) =
      case i of 1 -> (f x , a2)
                2 -> (a1 , f x)
                0 ->  if ind == 0 then (f a1,a2)
                                  else (a1,f a2)
                _ -> error "Incorect input"



-- | 'samples for testing'
sample0 :: Perfect Int
sample0 = Z 0

sample1 :: Perfect Int
sample1 = S (Z (1,2))

sample2 :: Perfect Int
sample2 = S (S(Z((1,2),(3,4))))

sample3 :: Perfect Int
sample3 = S(S (S(Z (((1,2),(3,4)),((5,6),(7,8))))))

testPer :: Perfect Int
testPer = build 10 [1..1025]

testPer1 :: Perfect Int
testPer1 = build 8 [1..10000]

testPer2 :: Perfect Char
testPer2 = build 3 ['a'..'z']

-- | This gets an error as list is smaller than the given expected size
testPer3 :: Perfect String
testPer3 = build 2 ["str1","str2","str3"]