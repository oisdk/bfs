module Data.Tree.BreadthFirst.Zippy where

import           Control.Monad.State.Simple
import           Data.Tree                     hiding (levels)
import           Data.Tree.BreadthFirst.Common
import           Control.Applicative

levels :: Tree a -> [[a]]
levels ts = f ts []
  where
    f (Node x xs) (q:qs) = (x:q) : foldr f qs xs
    f (Node x xs) []     = [x]   : foldr f [] xs

breadthFirst :: Applicative f
             => (a -> f b)
             -> Tree a
             -> f (Tree b)
breadthFirst c tr =
    head <$> foldr (liftA2 evalState) (pure []) (f tr [])
  where
    f (Node x xs) (q:qs) =
        app2 (\y ys zs -> Node y ys:zs) (c x) (fill xs) q : foldr f qs xs
    f (Node x xs) [] =
        map2 (\y ys -> [Node y ys]) (c x) (fill xs) : foldr f [] xs
