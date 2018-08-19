{-# Language DeriveFoldable #-}


module Task3  where

import System.IO
import Prelude
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

-- | a tree structure
data Tree a = Leaf a | Node (Tree a) (Tree a)  deriving (Show)


-- | Sample trees structures
sample_tree0 :: Tree [Char]
sample_tree0 = Leaf "00"

sample_tree1 :: Tree Int
sample_tree1 = Node (Leaf 1) (Leaf 2)

sample_tree2 :: Tree Char
sample_tree2 = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')


-- | assigns value to leaf
-- >>> assign_value 3 3 =Leaf (3,3)
assign_value :: a -> Int -> Tree (a,Int)
assign_value a x = Leaf (a, x)

-- | relabels a Tree and prints it
-- >>> sample_tree0 = Leaf ("00",0)
-- >>> relabelTree sample_tree1 =Node (Leaf (1,1)) (Leaf (2,2))

relabelTree :: Tree a -> IO (Tree (a, Int))
relabelTree tree = do
  nodeval <- newIORef 0
  let
    relabelTree' :: Tree a -> IO (Tree (a, Int))
    relabelTree' t = do
      x <- readIORef nodeval
      case t of Node t1 t2 -> do
                                res1 <- relabel t1
                                res2 <- relabel t2
                                return $ Node res1 res2
                Leaf a     -> do
                                return $ assign_value a x
    relabel :: Tree a -> IO (Tree (a,Int))
    relabel t = do
      modifyIORef nodeval (+ 1)
      x <- readIORef nodeval
      case t of Node t1 t2 -> do
                                res1<-relabel t1
                                res2<-relabel t2
                                return (Node res1 res2)
                Leaf a     -> do
                                return (assign_value a x)

  relabelTree' tree


-- | a code that crashes because of unsafePerformIO
anything :: IORef a
anything  = unsafePerformIO $ newIORef $ unsafePerformIO $ readIORef anything --this version gets seg fault
--anything  =  unsafePerformIO $ readIORef anything                          --this version loops forever

cast :: a -> b
cast a = unsafePerformIO $ test_anything' a --and it crashes


test_anything :: IO ()
test_anything = do
  writeIORef anything $ Just 3
  bang <- readIORef anything
  print (bang :: [Char])

test_anything' :: a -> IO b
test_anything' a = do
  writeIORef anything a
  unsafePerformIO $ unsafePerformIO $ readIORef anything

