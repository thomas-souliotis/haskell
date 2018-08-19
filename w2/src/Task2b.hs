{-# Language DeriveFoldable #-}



import System.IO
import Prelude 
import Control.Concurrent
import Control.Concurrent.MVar
import System.Environment 
import Control.Monad

main :: IO ()
main = do
   n <- getArgs
   sun (read (head n) :: Int)
--main ::  IO ()

sun :: Int -> IO ()
sun n = do
  xs <- replicateM n (newMVar ())
  mapM_ (forkIO  . (thread xs)) [1..n]
  threadDelay (n * 5000000000)


thread :: [MVar ()] -> Int -> IO ()
thread xs n = forever $ do
  if (n == length xs) then do
                            b <- takeMVar (head xs)
                            a <- takeMVar (xs !! (n-1))
                            putStrLn ("Philospher #" ++ (show n) ++ " ate") 
                            putMVar (head xs) ()
                            putMVar (xs !! (n-1)) ()
                    else do
                            a <- takeMVar (xs !! (n-1))
                            b <- takeMVar (xs!! n)
                            putStrLn ("Philospher #" ++ (show n) ++ " ate") 
                            putMVar (xs !! (n-1)) ()
                            putMVar (xs!! n) ()

