{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module Task2 where

import Data.Functor.Identity

data FunList a b t = Done t | More a (FunList a b (b -> t))

-- | 2.1
-- Functor instance of Funlist
instance Functor (FunList a b) where
    fmap :: (t -> t1) -> FunList a b t -> FunList a b t1
    fmap f (Done t)   = Done (f t)
    fmap f (More a g)  = More a $ fmap  (\b -> f . b) g

-- | Applicative instance of Funlist
instance Applicative (FunList a b) where
    pure :: t -> FunList a b t
    pure = Done

    (<*>) :: FunList a b (t1 -> t2) -> FunList a b t1 -> FunList a b t2
    Done f      <*> Done a     = Done $ f a
    Done f      <*> (More a b) = More a $ fmap (\g -> f.g) b
    (More a f)  <*> Done b     = More a $ fmap (\g a' b' -> g b' a') f <*> Done b
    (More a f)  <*> (More b c) = More a $ fmap (\g a' b' -> g b' a') f <*> More b c

-- | 2.2

-- | A function from FunList to list.
toList :: FunList a b t -> [a]
toList (Done _)        = []
toList (More a b) = a : toList b

-- | The reverse of toList.
fromList :: [a] -> FunList a b [b]
fromList []     = Done []
fromList (x:xs) = More x $ const <$> fromList xs

-- >>> (toList . fromList) [1,2,3] == id [1,2,3]
-- True

-- | 2.3
-- A singleton FunList.
singleton :: a -> FunList a b b
singleton a = More a f
  where
    f = flip const <$> Done ()

-- | It gets the funlist as an element.
fuse :: FunList a a t -> t
fuse (Done a)   = a
fuse (More a g) = fuse $ (\f -> f a) <$> g

-- >>> (fuse . singleton) False == False
-- True

-- | 2.4
-- The Traversal full description.
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> (s -> f t)

type Traversal' s a = Traversal s s a a

over :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
over sa f s = runIdentity (sa (Identity . f) s)

set :: ((a -> Identity b) -> s -> Identity t) -> s -> b -> t
set sa s a = over sa (const a) s

each :: Traversable t => Traversal (t a) (t b) a b
each = traverse

both :: Traversal (a, a) (b, b) a b
both f (a, a') = (,) <$> f a <*> f a'

ignored :: Traversal' s a
ignored = const pure
-- | Everything until here was imported from Traversal.hs and is helpful for next.

-- | Function from Traversal to Funlist.
toFunList :: Traversal s t a b -> (s -> FunList a b t)
toFunList t = t singleton

-- | Reverse of toList.
fromFunList :: (s -> FunList a b t) -> Traversal s t a b
fromFunList f = flip $ flip fromFunList' . f

-- | A helper function for fromFunList.
fromFunList' :: Applicative f => (a -> f b) -> FunList a b t -> f t
fromFunList' _ (Done t)   = pure t
fromFunList' f (More a b) = fromFunList' f b  <*> f a

-- | 2.5
-- transform transforms a transformation on FunLists into a transformation on Traversals
transform :: (FunList a b t -> FunList a b t) -> (Traversal s t a b -> Traversal s t a b)
transform f t = fromFunList  (f <$> toFunList t)


-- | A function from  a Traversal' to a Traversal'. Its use is shown below.
-- >>> set (heading each) "Athens" 'x'
-- "xthens"
heading :: Traversal' s a -> Traversal' s a
heading  = transform f
  where
    f (Done a)   = Done a
    f (More a b) = More a (heading' b)

-- | Helper function of heading
heading' :: FunList a a t -> FunList a a t
heading' (Done a) = Done a
heading' (More a b) = heading' $ fmap (\f-> f a) b

-- | A function from  a Traversal' to a Traversal'. Its use is shown below.
-- >>> set (tailing each) "Athens" 'x'
-- "Axxxxx"
tailing :: Traversal' s a -> Traversal' s a
tailing = transform f
  where
    f (Done a)   = Done a
    f (More a b) = fmap (\g -> g a) b

-- | A function from  a Traversal' to a Traversal'. Its use is shown below.
-- >>> set (taking 3 each) "Athens" 'x'
-- "xxxens"
taking :: Int -> Traversal' s a -> Traversal' s a
taking n = transform $ f n
  where
    f  :: Int -> FunList a a t -> FunList a a t
    f 0 anything = heading' anything
    f _ (Done a) = Done a
    f n' (More a b) = More a $ f (n'-1) b

-- | A function from  a Traversal' to a Traversal'. Its use is shown below.
-- >>> set (dropping 3 each) "Athens" 'x'
-- "Athxxx"
dropping :: Int -> Traversal' s a -> Traversal' s a
dropping n = transform $ f n
  where
    f  :: Int -> FunList a a t -> FunList a a t
    f 0 anything = anything
    f _ (Done a) = Done a
    f n' (More a b) = f (n'-1) $ fmap (\g -> g a) b

-- | A function from  a Traversal' to a Traversal'. Its use is shown below.
-- >>> set (filtering (< 'm') each) "Athens" 'x'
-- "xtxxns"
filtering :: (a -> Bool) -> Traversal' s a -> Traversal' s a
filtering p = transform  $ f p
  where
    f :: (a -> Bool) -> FunList a a t -> FunList a a t
    f _ (Done a) = Done a
    f p' (More a b) = if p' a then More a $ f p' b
                              else f p' $ (\g-> g a) <$> b

-- | A function from  a Traversal' to a Traversal'. Its use is shown below.
-- >>> set (element 1 each) "Athens" 'x'
-- "Axhens"
element :: Int -> Traversal' s a -> Traversal' s a
element n = transform $ f n
  where
    f :: Int -> FunList a a t -> FunList a a t
    f _ (Done a)   = Done a
    f 0 (More a b) = More a $ heading' b
    f n' (More a b) = f (n'-1) $ fmap (\g -> g a) b

