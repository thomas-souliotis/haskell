{-# Language DeriveFoldable #-}



import System.IO
import Prelude
import Control.Concurrent
import System.Environment
import Control.Monad
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM


-- | the Philospher task with thrreads and TVar
main :: IO ()
main = do
   n <- getArgs
   sun (read (head n) :: Int)

sun :: Int -> IO ()
sun n = do
  forks  <- replicateM n (newTVarIO False)
  mapM_ (forkIO  . (thread forks)) [1..n]
  threadDelay (n * 5000000000)

thread forks n = forever $ do
  if (n == length forks) then do
                            atomically $ eat (forks !! (n-1)) (head forks)
                            putStrLn $  ("Philospher #" ++ (show n) ++ " ate")
                            atomically $ ate (forks !! (n-1)) (head forks)
                    else do
                            atomically $ eat (forks !! (n-1)) (forks !! n)
                            putStrLn $  ("Philospher #" ++ (show n) ++ " ate")
                            atomically $ ate (forks !! (n-1)) (forks !! n)
eat a b  = do
  fork1 <- readTVar a
  fork2 <- readTVar b
  when (fork1 || fork2) retry
  writeTVar a True
  writeTVar b True


ate a b = do
  writeTVar a False
  writeTVar b False

