{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Comonad.Cofree.BreadthFirst
  (breadthFirst
  ,ibreadthFirst)
  where

import           Control.Comonad.Cofree

import           Control.Applicative
import           Control.Applicative.Backwards
import           Control.Monad.State.Simple
import           Data.Functor.Compose
import           Data.Profunctor.Unsafe

breadthFirst
    :: forall t a f b. (Traversable t, Applicative f)
    => (a -> f b) -> Cofree t a  -> f (Cofree t b)
breadthFirst c (t:<ts) =
    liftA2
        evalState
        (map2 (:<) (c t) fill)
        (foldr (liftA2 evalState) (pure []) chld)
  where
    (fill,chld) = runState (levl ts) []

    levl :: t (Cofree t a) -> State [f (State [Cofree t b] [Cofree t b])] (State [Cofree t b] (t (Cofree t b)))
    levl = (forwards . getCompose) #. traverse (Compose #. Backwards #. f)

    f :: Cofree t a -> State [f (State [Cofree t b] [Cofree t b])] (State [Cofree t b] (Cofree t b))
    f (x:<xs) = do
        q <- State unconsMay
        r <- levl xs
        modify (app2 (\y ys zs -> (y:<ys) : zs) (c x) r q :)
        pure (State uncons)

    unconsMay (x:xs) = (x,xs)
    unconsMay [] = (pure (pure []), [])

uncons :: [a] -> (a, [a])
uncons  = \case
    (x:xs) -> (x,xs)
    []     -> errorWithoutStackTrace "uncons: empty list!"
{-# INLINE uncons #-}

-- breadthFirst
--     :: (Applicative f, Traversable t)
--     => (a -> f b) -> Cofree t a -> f (Cofree t b)
-- breadthFirst c (t:<ts) = liftA2 evalState (map2 (:<) (c t) (fill ts)) chld
--   where
--     chld = foldr (liftA2 evalState) (pure []) (foldr f [] ts)
--     fill = traverse (const (State uncons))
--     {-# INLINE chld #-}
--     {-# INLINE fill #-}

--     f (x:<xs) (q:qs) = app2 (\y ys zs -> (y:<ys) : zs) (c x) (fill xs) q : foldr f qs xs
--     f (x:<xs) []     = map2 (\y ys    -> [y:<ys]     ) (c x) (fill xs)   : foldr f [] xs

--     map2 k x xs = fmap   (\y -> fmap   (k y) xs) x
--     app2 k x xs = liftA2 (\y -> liftA2 (k y) xs) x

--     {-# INLINE map2 #-}
--     {-# INLINE app2 #-}
-- {-# INLINE breadthFirst #-}

ibreadthFirst
    :: (Applicative f, Traversable t)
    => (Int -> a -> f b) -> Cofree t a -> f (Cofree t b)
ibreadthFirst c (t:<ts) = liftA2 evalState (map2 (:<) (c 0 t) (fill ts)) chld
  where
    chld = foldr (liftA2 evalState) (pure []) (foldr (f 1) [] ts)
    {-# INLINE chld #-}
    fill = traverse (const (State uncons))
    {-# INLINE fill #-}
    f i (x:<xs) (q:qs) = app2 (\y ys zs -> (y:<ys) : zs) (c i x) (fill xs) q : foldr (f (i+1)) qs xs
    f i (x:<xs) []     = map2 (\y ys    -> [y:<ys]     ) (c i x) (fill xs)   : foldr (f (i+1)) [] xs

{-# INLINE ibreadthFirst #-}

map2 :: (Functor f, Functor g) => (a -> b -> c) -> f a -> g b -> f (g c)
map2 k x xs = fmap (\y -> fmap (k y) xs) x

app2 :: (Applicative f, Applicative g) => (a -> b -> c -> d) -> f a -> g b -> f (g c) -> f (g d)
app2 k x xs = liftA2 (\y -> liftA2 (k y) xs) x