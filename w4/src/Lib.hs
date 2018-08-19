module Lib
    ( validator',
      walletEnc',
      walletDec'
    ) where

import qualified Task4 as T4

validator' ::IO ()
validator' = do
  inp <-getLine
  putStrLn (T4.validator inp)

walletEnc' :: IO ()
walletEnc' = do
  putStrLn "Please give 0 if it is just a mainnet address, and 1 if we refer to a compressed public key. Testnet is not considered as an option as requested "
  i <- getLine
  putStrLn "Now give the private key"
  inp <-getLine
  putStrLn (T4.walletEnc i inp)

walletDec' :: IO ()
walletDec' = do
  putStrLn "Please give 0 if it is just a mainnet address, and 1 if we refer to a compressed public key. Testnet is not considered as an option as requested "
  i <- getLine
  putStrLn "Now give the WIF"
  inp <-getLine
  putStrLn (T4.walletDec i inp)