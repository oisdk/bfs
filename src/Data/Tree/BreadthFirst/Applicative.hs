module Data.Tree.BreadthFirst.Applicative where

import           Control.Applicative
import           Data.Tree                     hiding (levels)
import           Control.Applicative.Phases

breadthFirst
    :: Applicative f
    => (a -> f b) -> Tree a -> f (Tree b)
breadthFirst c = runPhasesForwards . go
  where
    go (Node x xs) = liftA2 Node (now (c x)) (delay (traverse go xs))
{-# INLINE breadthFirst #-}
