{-# LANGUAGE LambdaCase #-}

module Control.Comonad.BreadthFirst where

import           Control.Comonad.Cofree

import           Control.Applicative
import           Control.Monad.State.Simple
-- import           Data.Bifunctor
-- import           Data.Functor.Compose
-- import           Data.Monoid                (Endo (..))
-- import           Data.Profunctor.Unsafe

-- breadthFirst
--     :: (Traversable t, Applicative f)
--     => (a -> f b) -> Cofree t a  -> f (Cofree t b)
-- breadthFirst c (t :< ts) =
--     liftA2
--         evalState
--         (map2 (:<) (c t) fill)
--         (foldr (liftA2 evalState) (pure []) chld)
--   where
--     (chld,fill) = go [] ts
--     go b =
--         first (`appEndo` b) .
--         getCompose #.
--         traverse (\x -> Compose (Endo (f x), State (\(y:ys) -> (y, ys))))
--     f (x :< xs) (q:qs) = app2 (\y ys zs -> (y :< ys) : zs) (c x) r q : rs where (rs,r) = go qs xs
--     f (x :< xs) []     = map2 (\y ys    -> [y :< ys]     ) (c x) r   : rs where (rs,r) = go [] xs
--     map2 = flip . (fmap .) . flip . (fmap .)
--     app2 = flip . (liftA2 .) . flip . (liftA2 .)

breadthFirst
    :: (Applicative f, Traversable t)
    => (a -> f b) -> Cofree t a -> f (Cofree t b)
breadthFirst c (t :< ts) = liftA2 evalState (map2 (:<) (c t) (fill ts)) chld
  where
    chld = foldr (liftA2 evalState) (pure []) (foldr f [] ts)
    {-# INLINE chld #-}
    fill = traverse (const (State (\case
                                        (x:xs) -> (x, xs)
                                        [] -> errorWithoutStackTrace
                                                  "Control.Comonad.BreadthFirst: bug!")))
    {-# INLINE fill #-}
    f (x :< xs) (q:qs) = app2 (\y ys zs -> (y :< ys) : zs) (c x) (fill xs) q : foldr f qs xs
    f (x :< xs) []     = map2 (\y ys    -> [y :< ys]     ) (c x) (fill xs)   : foldr f [] xs

    map2 k x xs = fmap   (\y -> fmap   (k y) xs) x
    app2 k x xs = liftA2 (\y -> liftA2 (k y) xs) x

    {-# INLINE map2 #-}
    {-# INLINE app2 #-}
{-# INLINE breadthFirst #-}
