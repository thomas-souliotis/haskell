module Task3 where

import           Control.DeepSeq
import           Criterion
import           Criterion.Main
import qualified Task2          as T2

-- | Will make benchmarks for a list of n's, for the first, the middle and the last element
main :: IO ()
main = defaultMain [
  bgroup " Indexing first element " [mkBenchmark i n | n <- [1..10]++ [15, 20], i <-[0]],
  bgroup " Indexing medium element " [mkBenchmark i n | n <- [1..10]++ [15, 20], i <-[2^(n-1)-1]],
  bgroup " Indexing last element " [mkBenchmark i n | n <- [1..10]++ [15, 20], i <-[2^n-1]]
  ]

-- | Responsible for the first element
mkBenchmark ::Int -> Int -> Benchmark
mkBenchmark i n= env (mkEnv n) $ \s ->
    bench ("Indexing the element of a Perfect with n= " ++ show n) $ nf (f s) i
  where
    f :: T2.Perfect Int -> Int -> Maybe Int
    f = T2.index

-- | Creates the environment by building the tree for each n. We build only Perfect Int but that is not necessary!
-- | We could easily implement for any arbitary a with being NFdata.
mkEnv :: Int -> IO (T2.Perfect Int)
mkEnv n = return (T2.build n [1..(2^n)])

-- | An instance for Perfect a, but we could have implemented it better in Task2. However, there is no problem.
instance NFData a => NFData (T2.Perfect a)