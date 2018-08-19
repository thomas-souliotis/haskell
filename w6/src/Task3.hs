{-#LANGUAGE RankNTypes #-}

module Task3 where

data FunctorDict f = FunctorDict { fmap_ :: forall a b . (a -> b) -> f a -> f b }

listFunctor :: FunctorDict []
listFunctor = FunctorDict map

data Sum f g a = Inl (f a) | Inr (g a)
newtype Compose f g a = Compose { getCompose :: f (g a) }

instance (Functor f, Functor g) => Functor (Sum f g)
instance (Functor f, Functor g) => Functor (Compose f g)


-- | A dictionary-transforming sum function
sumFunctor :: FunctorDict f -> FunctorDict g -> FunctorDict (Sum f g)
sumFunctor f g  = FunctorDict fmap_'
  where
    fmap_' a (Inl b) = Inl $ (fmap_ f) a b
    fmap_' a (Inr b) = Inr $ (fmap_ g) a b

-- | A dictionary-transforming compose function
composeFunctor :: FunctorDict f -> FunctorDict g -> FunctorDict (Compose f g)
composeFunctor f g = FunctorDict fmap_'
  where
    fmap_' a (Compose b) = Compose $ (fmap_ f) ((fmap_ g) a) b

