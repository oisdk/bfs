module Control.Monad.State.Simple where

import           Control.Applicative
import           Control.Monad.Fix

newtype State s a
    = State
    { runState :: s -> (a, s)
    }

instance Functor (State s) where
    fmap f xs =
        State
            (\s ->
                  case runState xs s of
                      (x,s') -> (f x, s'))
    {-# INLINE fmap #-}

instance Applicative (State s) where
  pure x = State (\s -> (x, s))
  {-# INLINE pure #-}
  fs <*> xs = State (\s -> case runState fs s of
    (f, s') -> case runState xs s' of
      (x, s'') -> (f x, s''))
  {-# INLINE (<*>) #-}
  liftA2 f xs ys = State (\s -> case runState xs s of
                             (x, s') -> case runState ys s' of
                               (y, s'') -> (f x y, s''))
  {-# INLINE liftA2 #-}


instance Monad (State s) where
  xs >>= f = State (uncurry (runState . f) . runState xs)

evalState :: State s a -> s -> a
evalState xs = fst . runState xs
{-# INLINE evalState #-}

execState :: State s a -> s -> s
execState xs = snd . runState xs
{-# INLINE execState #-}

get :: State s s
get = State (\s -> (s, s))
{-# INLINE get #-}

put :: s -> State s ()
put x = State (const ((), x))
{-# INLINE put #-}

modify :: (s -> s) -> State s ()
modify f = State (\s -> ((), f s))
{-# INLINE modify #-}

gets :: (s -> a) -> State s a
gets f = State (\s -> (f s, s))
{-# INLINE gets #-}

instance MonadFix (State s) where
    mfix f = State (\s -> let (x,s') = runState (f x) s in (x,s'))
    {-# INLINE mfix #-}
