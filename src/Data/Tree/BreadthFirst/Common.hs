{-# options_ghc -fno-warn-incomplete-uni-patterns #-}

module Data.Tree.BreadthFirst.Common where

import           Control.Monad.State.Simple
import           Control.Applicative


map2
    :: (Functor f, Functor g)
    => (a -> b -> c) -> f a -> g b -> f (g c)
map2 f x xs =
    fmap (\y -> fmap (f y) xs) x
{-# INLINE map2 #-}

app2
    :: (Applicative f, Applicative g)
    => (a -> b -> c -> d) -> f a -> g b -> f (g c) -> f (g d)
app2 f x xs =
    liftA2 (\y -> liftA2 (f y) xs) x
{-# INLINE app2 #-}

fill :: Traversable t => t a -> State [b] (t b)
fill = traverse (const (State (\(x:xs) -> (x, xs))))
{-# INLINE fill #-}
