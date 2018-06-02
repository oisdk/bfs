{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Comonad.Cofree.BreadthFirst
  (breadthFirst
  ,breadthFirst2
  ,breadthFirst3
  ,levels1
  ,levels2
  ,levels3)
  where

import           Control.Applicative
import           Control.Comonad.Cofree
import           Control.Monad.State.Simple
import           Data.Functor.Compose
import           Data.List                  (unfoldr)
import           Data.Monoid

uncons :: [a] -> (a, [a])
uncons  = \case
    (x:xs) -> (x,xs)
    []     -> errorWithoutStackTrace "uncons: empty list!"
{-# INLINE uncons #-}

breadthFirst
    :: (Applicative f, Traversable t)
    => (a -> f b) -> Cofree t a -> f (Cofree t b)
breadthFirst c tr = fmap head (go [tr])
  where
    go [] = pure []
    go xs = liftA2 evalState zs (go (ys []))
      where
        Compose (Endo ys,Compose zs) = traverse f xs
    f (x :< xs) =
        Compose
            (Endo (flip (foldr (:)) xs), Compose (map2 (:<) (c x) (fill xs)))

breadthFirst2
    :: (Applicative f, Traversable t)
    => (a -> f b) -> Cofree t a -> f (Cofree t b)
breadthFirst2 c t = head <$> go [t]
  where
    go [] = pure []
    go xs =
        liftA2
            evalState
            (getCompose (traverse f xs))
            (go
                 (foldr
                      (\(_ :< ys) b ->
                            foldr (:) b ys)
                      []
                      xs))
    f (x :< xs) = Compose (map2 (:<) (c x) (fill xs))

breadthFirst3
    :: forall t a f b. (Applicative f, Traversable t)
    => (a -> f b) -> Cofree t a -> f (Cofree t b)
breadthFirst3 c t =
    fmap head (f b t e [])
  where
    f k (x:<xs) ls qs = k (app2 (\y ys zs -> (y:<ys):zs) (c x) (fill xs) ls) (xs:qs)

    b _ [] = pure []
    b k qs = liftA2 evalState k (foldl (foldl f) b qs e [])

    e = pure (pure [])
{-# INLINE breadthFirst #-}

-- breadthFirst2
--     :: (Applicative f, Traversable t)
--     => (a -> f b) -> Cofree t a -> f (Cofree t b)
-- breadthFirst2 c (t:<ts) = liftA2 evalState (map2 (:<) (c t) (fill ts)) chld
--   where
--     chld = foldr (liftA2 evalState) (pure []) (foldr f [] ts)
--     {-# INLINE chld #-}

--     f (x:<xs) (q:qs) = app2 (\y ys zs -> (y:<ys) : zs) (c x) (fill xs) q : foldr f qs xs
--     f (x:<xs) []     = map2 (\y ys    -> [y:<ys]     ) (c x) (fill xs)   : foldr f [] xs
map2
    :: (Functor f, Functor g)
    => (a -> b -> c) -> f a -> g b -> f (g c)
map2 f x xs =
    fmap (\y -> fmap (f y) xs) x

app2
    :: (Applicative f, Applicative g)
    => (a -> b -> c -> d) -> f a -> g b -> f (g c) -> f (g d)
app2 f x xs =
    liftA2 (\y -> liftA2 (f y) xs) x

{-# INLINE map2 #-}
{-# INLINE app2 #-}

fill :: Traversable t => t a -> State [b] (t b)
fill = traverse (const (State uncons))
{-# INLINE fill #-}

levels1 :: Cofree [] a -> [[a]]
levels1 t =  unfoldr (f . concat) [[t]]
  where
    f [] = Nothing
    f xs = Just (unzip [(y,ys) | y :< ys <- xs])
{-# INLINE levels1 #-}

levels2 :: Cofree [] a -> [[a]]
levels2 ts = f b ts [] []
  where
    f k (x:<xs) ls qs = k (x : ls) (xs : qs)

    b _ [] = []
    b k qs = k : foldl (foldl f) b qs [] []
{-# INLINE levels2 #-}

levels3 :: Cofree [] a -> [[a]]
levels3 ts = f ts []
  where
    f (x:<xs) (l:qs) = (x:l) : foldr f qs xs
    f (x:<xs) []     = [x]   : foldr f [] xs
{-# INLINE levels3 #-}

