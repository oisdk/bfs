{-# LANGUAGE ScopedTypeVariables #-}

module Data.Tree.BreadthFirst.Queued
  (levels
  ,breadthFirst
  ,unfold)
  where

import           Control.Applicative
import           Control.Applicative.Backwards
import           Control.Monad.State.Simple
import           Data.Tree                     hiding (levels, unfoldForest)
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

unfold :: Monad m => (b -> m (a, [b])) -> b -> m (Tree a)
unfold f b = f b >>= \(x,xs) -> fmap (Node x) (unfoldForest f xs)

-- unfoldForest :: Monad m => (b -> m (a, [b])) -> [b] -> m (Forest a)
-- unfoldForest f ts = b [ts] (const id)
--   where
--     b [] k = pure (k (pure []) [])
--     b qs k = foldl (foldr t) b qs [] (\x xs -> k (pure []) (evalState x xs))

--     t a fw bw k = do
--         (x,cs) <- f a
--         fw (cs : bw) (k . liftA2 (:) (fmap (Node x) (fill cs)))

unfoldForest :: Monad m => (b -> m (a, [b])) -> [b] -> m (Forest a)
unfoldForest f ts = b [ts] (\ls -> concat . foldr run id ls)
  where
    b [] k = pure (k [] [])
    b qs k = foldl g b qs [] (\ls -> k [] . foldr run id ls)

    g a xs qs k = foldr t (\ls ys -> a ys (k . (:) ls)) xs (pure id) qs

    t a fw xs bw = f a >>= \(x,cs) -> fw (liftA2 (.) xs (fmap (:) (pop x))) (cs:bw)

    run x xs ys = uncurry (:) $ runState (x <*> (State (\zs -> ([], xs zs))) ) ys
        -- x <- x'
        -- xs <- xs'
        -- modify ((x xs):)
        -- pure []

    go st = uncurry (:) . runState st

    pop x  = State (\(y:ys) -> (Node x y, ys))
