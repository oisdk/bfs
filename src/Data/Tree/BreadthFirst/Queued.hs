module Data.Tree.BreadthFirst.Queued where

import           Control.Applicative
import           Control.Monad.State.Simple
import           Data.Tree                     hiding (levels)
import           Data.Tree.BreadthFirst.Common

levels :: Tree a -> [[a]]
levels tr = f b tr [] []
  where
    f k (Node x xs) ls qs = k (x : ls) (xs : qs)

    b _ [] = []
    b k qs = k : foldl (foldl f) b qs [] []
{-# INLINE levels #-}

breadthFirst
    :: Applicative f
    => (a -> f b) -> Tree a -> f (Tree b)
breadthFirst c tr = fmap head (f b tr e [])
  where
    f k (Node x xs) ls qs =
        k (app2 (\y ys zs -> Node y ys : zs) (c x) (fill xs) ls) (xs : qs)
    b _ [] = pure []
    b l qs = liftA2 evalState l (foldl (foldl f) b qs e [])
    e = pure (pure [])
{-# INLINE breadthFirst #-}
