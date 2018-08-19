{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module Task3 where


import Numeric.Natural
import Data.Profunctor.Choice
import Data.Profunctor
import Data.Tagged
import Data.Functor.Identity
--import Data.Either
import Data.Functor.Const    (Const (..))
import Data.Monoid           (First (..))
import qualified Task2 as T2

type Prism' s a = Prism s s a a

prism' :: (s -> Maybe a) -> (a -> s) -> Prism' s a
prism' pr = prism pr'
  where
    pr' s = case pr s of
      Nothing -> Left s
      Just a -> Right a

type Prism s t a b = forall f p . (Applicative f, Choice p) => p a (f b) -> p s (f t)

review :: Prism' s a -> a -> s
review p a = runIdentity (unTagged (p (Tagged (Identity a))))

preview :: ((a -> Const (First a) a) -> s -> Const (First a) s) -> s -> Maybe a
preview p = getFirst . getConst . p (Const . First . Just)

prism :: (s -> Either t a) -> (b -> t) -> Prism s t a b
prism pr rv p = dimap pr (either pure (fmap rv)) (right' p)

-- | 3.1 _Natural is implemented as requested
-- >>> preview _Natural 42
-- Just 42
-- >>> preview _Natural (-7)
-- Nothing
_Natural :: Prism' Integer Natural
_Natural = prism' pr f
  where
    pr x = if x >0 then Just $ fromIntegral x :: Maybe Natural
                   else Nothing
    f x = fromIntegral x :: Integer

-- | 3.2 _TheOne implementation: Given an a, the resulting prism’s focus should be the given element
-- >>> preview (_TheOne 'x') 'x'
-- Just ()
-- >>> preview (_TheOne 'x') 'y'
-- Nothing
-- >>> review (_TheOne 'x') ()
-- 'x'
_TheOne :: Eq a => a -> Prism' a ()
_TheOne x = prism' pr f
  where
    pr x' = if x == x' then Just ()
                       else Nothing
    f  _  = x

-- | 3.3
newtype Checked a = Checked { unChecked :: a } deriving Show

-- |  _Check finds only elements that fulfill the given predicate.
-- >>> preview (_Check odd) 42
-- Nothing
-- >>> preview (_Check odd) 17
-- Just (Checked {unChecked = 17})
-- >>> review (_Check odd) (Checked 3)
-- 3
_Check :: (a -> Bool) -> Prism' a (Checked a)
_Check g = prism' pr f
  where
    pr x = if g x then Just $ Checked x
                  else Nothing
    f  = unChecked


-- | 3.4, the filtering function from Task2

filtering'  :: (a -> Bool) -> T2.Traversal' s a -> T2.Traversal' s a
filtering' f traversal = (traversal . _Check f) . f'
  where
    f' :: (Applicative f) =>  (a -> f a) -> Checked a -> f (Checked a)
    f' a b = Checked <$> a (unChecked b)