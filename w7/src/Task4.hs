{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveTraversable #-}

module Task4 where


import Data.Functor.Const    (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Monoid           (First (..))
import Control.Monad
import Control.Monad.State
--import Data.Traversable
--import Data.Profunctor.Choice (Choice (..))
--import Data.Either            (either)
--import Data.Profunctor        (Profunctor (..))
--import Data.Tagged            (Tagged (..))


-- | From now until stated, everything is imported from Traversal.hs
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> (s -> f t)

type Traversal' s a = Traversal s s a a

over :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
over sa f s = runIdentity (sa (Identity . f) s)

view :: ((a -> Const a b) -> s -> Const a t) -> s -> a
view sa s = getConst (sa Const s)

toListOf :: ((a -> Const [a] a) -> s -> Const [a] s) -> s -> [a]
toListOf p = getConst . p (Const . return)

preview :: ((a -> Const (First a) a) -> s -> Const (First a) s) -> s -> Maybe a
preview p = getFirst . getConst . p (Const . First . Just)

set :: ((a -> Identity b) -> s -> Identity t) -> s -> b -> t
set sa s a = over sa (const a) s

each :: Traversable t => Traversal (t a) (t b) a b
each = traverse

both :: Traversal (a, a) (b, b) a b
both f (a, a') = (,) <$> f a <*> f a'

ignored :: Traversal' s a
ignored = const pure
-- | Everything till here was imported from Traversal.hs


-- | The Tree data structure with all the derivations needed
data Tree a = Tip | Node (Tree a) a (Tree a) deriving (Show, Functor, Foldable, Traversable)

-- | The inorder implementation
inorder :: Traversal (Tree a) (Tree b) a b
inorder _ Tip            = pure Tip
inorder f (Node l val r) = Node <$> postorder f l <*> f val <*> postorder f r

-- | Pre order implemantation
preorder :: Traversal (Tree a) (Tree b) a b
preorder _ Tip            = pure Tip
preorder f (Node l val r) = flip Node <$> f val <*> preorder f l <*> preorder f r

-- | Post order implemantation
postorder :: Traversal (Tree a) (Tree b) a b
postorder _ Tip            = pure Tip
postorder f (Node l val r) = helpF Node <$> postorder f l <*> postorder f r  <*> f val
  where helpF g a b c = g a c b


-- 4.2
-- | A function that traverses a tree with a predifined way and prints its vals the way they are processed
-- >>>printNodes inorder tree
-- 'a'
-- 'b'
-- 'c'
-- 'd'
-- >>>printNodes preorder tree
-- 'c'
-- 'b'
-- 'a'
-- 'd'
-- >>>printNodes postorder tree
-- 'a'
-- 'b'
-- 'd'
-- 'c'
printNodes :: Show a => Traversal' (Tree a) a -> Tree a -> IO ()
printNodes _ Tip   = putStrLn "Tip"
printNodes traversal tr = void $ traversal f tr
  where
    f x = print x >> return x


-- | A Function that labe a tree's nodes starting from 1.
-- >>> labelNodes inorder tree
-- Node (Node (Node Tip ('a',1) Tip) ('b',2) Tip) ('c',3) (Node Tip ('d',4) Tip)
-- >>> labelNodes postorder exTree3
-- Node (Node Tip (1,1) Tip) (0,3) (Node Tip (2,2) Tip)
-- >>> labeNodes preorder exTree2
-- Node Tip (0,1) Tip
labelNodes :: Traversal (Tree a) (Tree (a, Int)) a (a, Int) -> Tree a -> Tree (a, Int)
labelNodes traversal tr = evalState (traversal f tr) 1
  where
    f x = do
      n <- get
      put (n+1)
      return (x,n)

-- | Some example trees
-- The completely empty Tree only consisting of a Tip
exTree1 :: Tree a
exTree1 = Tip

-- | A tree with only one node
exTree2 :: Tree Int
exTree2 = Node Tip 0 Tip

-- | A slightly bigger tree with a parrent node and two more childe nodes.
exTree3 :: Tree Int
exTree3 = Node (Node Tip 1 Tip) 0 (Node Tip 2 Tip)

-- | A tree given as an example
tree :: Tree Char                             --           c
tree = Node                                   --          / \
            (Node                             --         /   \
                (Node Tip 'a' Tip)            --         b    d
                'b'                           --        / \  / \
                Tip)                          --       /
            'c'                               --      a
            (Node Tip 'd' Tip)                --     / \