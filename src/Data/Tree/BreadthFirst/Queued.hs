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
import           Control.Monad.Cont
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
unfold f ts = b [[ts]] (\ls -> head . head . execState ls)
  where
    b [] k = pure (k (pure ()) [])
    b qs k = foldl g b qs [] (\ls -> k (pure ()) . execState ls)

    g a xs qs k = runContT (foldlM (flip t) (pure id,qs) xs) (\(ls,ys) -> a ys (k . run ls))

    t a (xs,bw) = fmap (\(x,cs) -> (liftA2 (.) xs (fmap (:) (pop x)) ,cs:bw)) (lift (f a))

    run x xs = do
        y <- x <*> pure []
        () <- xs
        modify (y:)

    pop x  = State (\(y:ys) -> (Node x y, ys))
