module Data.Tree.BreadthFirst.Iterative where

import           Control.Applicative
import           Control.Monad.State.Simple
import           Data.Functor.Compose
import           Data.List                     (unfoldr)
import           Data.Tree                     hiding (levels)
import           Data.Tree.BreadthFirst.Common

levels :: Tree a -> [[a]]
levels t =  unfoldr (f . concat) [[t]]
  where
    f [] = Nothing
    f xs = Just (unzip [(y,ys) | Node y ys <- xs])
{-# INLINE levels #-}

breadthFirst
    :: Applicative f
    => (a -> f b) -> Tree a -> f (Tree b)
breadthFirst c tr = fmap head (go [tr])
  where
    go [] = pure []
    go xs =
        liftA2
            evalState
            (getCompose (traverse f xs))
            (go (foldr (\(Node _ ys) b -> foldr (:) b ys) [] xs))
    f (Node x xs) = Compose (map2 Node (c x) (fill xs))
{-# INLINE breadthFirst #-}

unfold :: Monad m => (b -> m (a, [b])) -> b -> m (Tree a)
unfold c tr = go head [tr]
  where
    go k [] = pure (k [])
    go k xs = do
        ys <- traverse c xs
        go
            (k . evalState (traverse f ys))
            [ z
            | (_,zs) <- ys
            , z <- zs ]
    f (x,xs) = fmap (Node x) (fill xs)
{-# INLINE unfold #-}
