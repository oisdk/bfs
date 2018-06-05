{-# LANGUAGE ScopedTypeVariables #-}

module Data.Tree.BreadthFirst.Queued
  (levels
  ,breadthFirst
  ,unfold)
  where

import           Control.Applicative
import           Control.Monad.State.Simple
import           Data.Tree                     hiding (levels)
import           Data.Tree.BreadthFirst.Common
import           Data.Foldable


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

unfold :: Monad m => (b -> m (a, [b])) -> b -> m (Tree a)
unfold f ts = b ([[ts]],\ls -> head . head . execState ls)
  where
    b ([],k) = pure (k (pure ()) [])
    b (qs,k) = foldrM g ([],\ls -> k (pure ()) . execState ls) qs >>= b

    g xs (qs,k) = fmap ((k .) . run) <$> foldlM (flip t) (qs,pure id) xs

    t a (bw,xs) = fmap (\(x,cs) -> (cs:bw,liftA2 (.) xs (fmap (:) (pop x)))) (f a)

    run x xs = do
        y <- x <*> pure []
        () <- xs
        modify (y:)

    pop x  = State (\(y:ys) -> (Node x y, ys))
