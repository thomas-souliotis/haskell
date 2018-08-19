{-# Language DeriveFoldable #-}


module Task5  where

import System.IO()
import Prelude hiding(id)
--import Data.IORef
import Control.Monad
import Data.Map as M


-- | Multiple data structures about ids, inputs and outputs
data Transaction =
  Transaction
    { tId      :: Id
    , tInputs  :: [Input]
    , tOutputs :: [Output]
    }
  deriving (Show)

type Id = Int

data Output =
  Output
    { oValue   :: Int
    , oAddress :: Address
    }
  deriving (Show)

type Address = String


data Input =
  Input
    { iPrevious :: Id
    , iIndex    :: Index
    }
  deriving (Show, Eq, Ord)

type Index = Int


type UTXOs = M.Map Input Output

-- | processes Transactions and UTXOs and outpus an Either structure
processTransaction :: Transaction -> UTXOs -> Either String UTXOs
processTransaction (Transaction id inps outps) mapper =
  if validation inps outps mapper then
    if summer inps outps mapper then let
                                         mapper' = delete' inps mapper
                                       in
                                         Right (insertinmap outps id 0 mapper')
                                else  Left "validity error2"
                                  else Left "validity error1"

delete' :: Ord k => [k] -> Map k a -> Map k a
delete' []     mapper = mapper
delete' (x:xs) mapper = delete' xs (M.delete x mapper)

validation :: [Input] -> [Output] -> M.Map Input Output -> Bool
validation []     _  _      = True
validation (x:xs) ys mapper = case M.lookup x mapper of Nothing -> False
                                                        Just _  -> validation xs ys (M.delete x mapper)

summer :: [Input] -> [Output] -> M.Map Input Output -> Bool
summer inps outps mapper = sum1 inps mapper >= sum2 outps

sum1 ::  [Input] -> M.Map Input Output -> Int
sum1 []     _      = 0
sum1 (x:xs) mapper = case M.lookup x mapper of Nothing              -> undefined
                                               Just (Output val _)  -> val + sum1 xs mapper

sum2 :: [Output] -> Int
sum2 []                  = 0
sum2 ((Output val _):ys) = val + sum2 ys

insertinmap :: [Output] -> Int -> Int -> Map Input Output -> Map Input Output
insertinmap []     _  _     mapper = mapper
insertinmap (x:xs) id index mapper = insertinmap xs id (index+1) (M.insert (Input id index) x mapper)


processTransactions :: [Transaction] -> UTXOs -> Either String UTXOs
processTransactions [] mapper = Right mapper
processTransactions (x:xs) mapper = case processTransaction x mapper of Right mapper'         -> processTransactions xs mapper'
                                                                        Left "validity error" ->  Left "validity error"
                                                                        _                     -> undefined

--2.5.2
mapper0 :: Map k a
mapper0                      = empty

outps0 :: [Output]
outps0                       = [Output 100 "a", Output 200 "b", Output 300 "c"]

sampletx0 :: Transaction
sampletx0                    = Transaction 0 [] []

mapper1 :: Map Input Output
mapper1                      = insert' inps1 outps0 mapper0

inps1 :: [Input]
inps1                        = [Input 0 0, Input 0 1, Input 0 2]

insert' :: Ord k => [k] -> [a] -> Map k a -> Map k a
insert' []     _      mapper =  mapper
insert' (x:xs) (y:ys) mapper = insert' xs ys (M.insert x y mapper)
insert' _       _     _      = undefined

sampletx1 :: Transaction
sampletx1                    = Transaction 1 [] outps0

sampletx2 :: Transaction
sampletx2                    = Transaction 2 inps1 outps1

sampletx3 :: Transaction
sampletx3                    = Transaction 3 [Input 2 0] [Output 100 "aaa"]

outps1 :: [Output]
outps1                       = [Output 101 "aa"]

iii :: Either String UTXOs
iii                          =  processTransaction sampletx2 mapper1

mapper2 :: Map Input Output
mapper2                      = (fromList [(Input {iPrevious = 2, iIndex = 0},Output {oValue = 101, oAddress = "aa"})])

iii2 :: Either String UTXOs
iii2                         =  processTransaction sampletx3 mapper2
--above is an initial check, the rest will follow--
testtxs :: Either String UTXOs
testtxs                      = processTransactions [sampletx2 , sampletx3] mapper1

--validation xs ys mapper = True

--2.5.3--
newtype ErrorState s a = ErrorState { runErrorState :: s -> Either String (a, s) }

instance Functor (ErrorState s) where
  fmap = liftM

instance Applicative (ErrorState s) where
  pure = return
  (<*>) = ap

instance Monad (ErrorState s) where
  return a               = ErrorState $ \s -> Right (a,s)
  ErrorState f  >>= cont = ErrorState $ \s -> case f s of Left a       -> Left a
                                                          Right (a,s') -> runErrorState (cont a) s'
 -- Right f >>= cont = \ s -> let (x, s') = f s in cont x s'

throwError :: String -> ErrorState s a
throwError str = ErrorState $ \_ -> Left str

get :: ErrorState s s
get = ErrorState $ \s -> Right (s,s)

put :: s -> ErrorState s ()
put s = ErrorState $ \_ -> Right ((), s)


--2.5.4--
processTransactions' :: [Transaction] -> ErrorState UTXOs ()
processTransactions' [] = throwError "None transaction given"
processTransactions' [x] = processTransaction' x
processTransactions' (x:xs) = do
  processTransaction' x
  processTransactions' xs


processTransaction' :: Transaction -> ErrorState UTXOs ()
processTransaction' (Transaction id inps outps)  = validation' inps inps outps id


validation' :: [Input] -> [Input] -> [Output] -> Id -> ErrorState (Map Input Output) ()
validation' []     inps outps id = summer' inps outps id
validation' (x:xs) inps outps id = do
     mapper <- get
     case M.lookup x mapper of Nothing ->  throwError "validity error"
                               Just _  ->  (validation' xs inps outps id)


summer' :: [Input] -> [Output] -> Id -> ErrorState (Map Input Output) ()
summer' inps outps id= do
   mapper <- get
   if sum1 inps mapper < sum2 outps then throwError "validation error2"
                                    else delete'' inps outps id

delete'' :: [Input] -> [a] -> Id -> ErrorState (Map Input a) ()
delete'' []     outps id = insertinmap' outps id 0
delete'' (x:xs) outps id = do
  mapper <- get
  put (M.delete x mapper)
  delete'' xs outps id

inserinmap' :: [Output] -> Int -> int -> ErrorState (Map Input a) ()
inserinmap' [] _ _ = throwError "trying to insert none existent txs"
inserinmap' _  _ _ = undefined

insertinmap' :: [a] -> Id -> Index -> ErrorState (Map Input a) ()
insertinmap' (x:[]) id index = do
  mapper <- get
  put (M.insert (Input id index) x mapper)

insertinmap' (x:xs) id index = do
  mapper <- get
  put (M.insert (Input id index) x mapper)
  insertinmap' xs id (index+1)

insertinmap' _ _ _            = undefined

testtxs' :: ErrorState UTXOs ()
testtxs'                      = processTransactions' [sampletx2 , sampletx3]



