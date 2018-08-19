{-# Language DeriveFoldable #-}
--Task 5--
import Data.Map
import System.IO
import Prelude hiding (empty)

data Trie a = Fork (Maybe a) (Map Char (Trie a)) deriving (Eq, Show, Foldable)

--instance Foldable Trie where
  --foldr = foldrBinTree

--instance Functor Trie where
  --fmap = mapBinTree
sample_tree = Fork Nothing
  (fromList
    [('b', Fork (Just 43)
             (fromList [('a', Fork Nothing
                                (fromList [('r', Fork (Just 2) empty)
                                          ,('z', Fork (Just 3) empty)
                                          ]
                                )
                        )
                       ]
             )
     )
    ,('f', Fork (Just 0)
             (fromList [('o', Fork Nothing
                                (fromList [('o', Fork (Just 1) empty)])
                        )
                       ]
             )
     )
    ]
  )

empty'  :: Trie a -- produces an empty trie
empty' = Fork Nothing empty

null' (Fork Nothing empty) = True
null' (Fork _ _) = False


checkelems (Fork a b) = elems b

checkrest (Fork a b) = b

checkkeys (Fork a b) =  keys b


--morecheck (Fork a b) = a : (elems b)

checkkeys' [] = ["finish"]
checkkeys' (x:xs) = ((checkkeys x) : (checkkeys'  xs)) ++ checkkeys'(checkelems x)

valid' (Fork a b)
  | ((a==Nothing) && (b==empty)) = True
  | otherwise = valid2 (elems b)

valid2 [] = True
valid2 (x:xs) =
  (valid3 x) && (valid2 xs) && (valid2 (checkelems x))

valid3 (Fork a b) = ((a,b)/=(Nothing,empty))

--valid' (Fork a b) = valid2 f
--valid' (Fork a empty) =
--valid' (Fork a b) =

--ftiakse to wste na vriskei ton swsto node kai na to vazei kai oxi opou nanai--

lookupaux key (Fork a b) =
  case Data.Map.lookup key b of Nothing         -> Nothing
                                Just (Fork a b) -> a

lookupaux2 key (Fork a b) =  Data.Map.lookup key b


sample_tree2 = Fork Nothing  (fromList [('r', Fork (Just 2) empty)])

lookup' :: String -> Trie a -> Maybe a -- looks up the value associated with the key
lookup' [] _ = Nothing
lookup' (x:xs) (Fork a b) =
  case xs of []        -> lookupaux x (Fork a b)
             otherwise -> case (lookupaux2 x (Fork a b)) of Nothing -> Nothing
                                                            Just t  -> lookup' xs t



