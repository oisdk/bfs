{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Comonad.Cofree.BreadthFirst
  (breadthFirst
  ,breadthFirst2
  ,ibreadthFirst
  ,levels1
  ,levels2
  ,levels3)
  where

import           Control.Applicative
import           Control.Comonad.Cofree
import           Control.Monad.State.Simple

uncons :: [a] -> (a, [a])
uncons  = \case
    (x:xs) -> (x,xs)
    []     -> errorWithoutStackTrace "uncons: empty list!"
{-# INLINE uncons #-}


breadthFirst
    :: forall t a f b. (Applicative f, Traversable t)
    => (a -> f b) -> Cofree t a -> f (Cofree t b)
breadthFirst c t =
    head <$> f b t (pure (pure [])) []
  where
    f k (x:<xs) ls qs = k (app2 (\y ys zs -> (y:<ys):zs) (c x) (fill xs) ls) (xs:qs)

    b k (q:qs) = liftA2 evalState k (foldl (foldl f) (foldl f b q) qs (pure (pure [])) [])
    b _ [] = pure []
{-# INLINE breadthFirst #-}

breadthFirst2
    :: (Applicative f, Traversable t)
    => (a -> f b) -> Cofree t a -> f (Cofree t b)
breadthFirst2 c (t:<ts) = liftA2 evalState (map2 (:<) (c t) (fill ts)) chld
  where
    chld = foldr (liftA2 evalState) (pure []) (foldr f [] ts)
    {-# INLINE chld #-}

    f (x:<xs) (q:qs) = app2 (\y ys zs -> (y:<ys) : zs) (c x) (fill xs) q : foldr f qs xs
    f (x:<xs) []     = map2 (\y ys    -> [y:<ys]     ) (c x) (fill xs)   : foldr f [] xs
{-# INLINE breadthFirst2 #-}

ibreadthFirst
    :: (Applicative f, Traversable t)
    => (Int -> a -> f b) -> Cofree t a -> f (Cofree t b)
ibreadthFirst c (t:<ts) = liftA2 evalState (map2 (:<) (c 0 t) (fill ts)) chld
  where
    chld = foldr (liftA2 evalState) (pure []) (foldr (f 1) [] ts)
    {-# INLINE chld #-}

    f i (x:<xs) (q:qs) = app2 (\y ys zs -> (y:<ys) : zs) (c i x) (fill xs) q : foldr (f (i+1)) qs xs
    f i (x:<xs) []     = map2 (\y ys    -> [y:<ys]     ) (c i x) (fill xs)   : foldr (f (i+1)) [] xs

{-# INLINE ibreadthFirst #-}

map2 :: (Functor f, Functor g) => (a -> b -> c) -> f a -> g b -> f (g c)
map2 k x xs = fmap (\y -> fmap (k y) xs) x

app2 :: (Applicative f, Applicative g) => (a -> b -> c -> d) -> f a -> g b -> f (g c) -> f (g d)
app2 k x xs = liftA2 (\y -> liftA2 (k y) xs) x

{-# INLINE map2 #-}
{-# INLINE app2 #-}

fill :: Traversable t => t a -> State [b] (t b)
fill = traverse (const (State uncons))
{-# INLINE fill #-}

levels1 :: Cofree [] a -> [[a]]
levels1 t =
    map (map rootLabel) $
        takeWhile (not . null) $
        iterate (concatMap subForest) [t]
  where
    rootLabel (x:<_) = x
    subForest (_:<xs) = xs

levels2 :: Cofree [] a -> [[a]]
levels2 ts = f b ts [] []
  where
    f k (x:<xs) ls qs = k (x : ls) (xs : qs)

    b _ [] = []
    b k qs = k : foldl (foldl f) b qs [] []

levels3 :: Cofree [] a -> [[a]]
levels3 tr = f tr [] where
  f (x:<xs) (y:ys) = (x:y) : foldr f ys xs
  f (x:<xs) []     = [x]   : foldr f [] xs
{-# INLINE levels3 #-}

