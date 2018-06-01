{-# options_ghc -fno-warn-orphans #-}

module Main (main) where

import Criterion.Main
import System.Random
import Control.Monad
import Control.Comonad.Cofree
import Control.Comonad.Cofree.BreadthFirst
import Control.Applicative
import Data.Monoid
import Control.DeepSeq
import Data.Functor.Identity
import Data.Bifunctor

int :: Int -> IO Int
int n = randomRIO (1,n)

instance (NFData1 f, NFData a) => NFData (Cofree f a) where
    rnf (x :< xs) = rnf x `seq` rnf1 xs
    {-# INLINE rnf #-}

cof :: Int -> Int -> IO (Cofree [] Int)
cof n m = go n
  where
    go i | i <= 1 = (:<[]) <$> int n
         | otherwise = liftA2 (:<) (int n) (replicateM m (go (i `div` m)))

constTrav :: Traversable f =>  Cofree f Int -> Int
constTrav = getSum . getConst . breadthFirst (Const . Sum)

identityTrav :: Traversable f => Cofree f a -> Cofree f a
identityTrav = runIdentity . breadthFirst Identity

fullTrav :: Traversable f => Cofree f Int -> (Int, Cofree f Int)
fullTrav = first getSum . breadthFirst (\x -> (Sum x, x+1))

benchAtSize :: Int -> [Int] -> Benchmark
benchAtSize n ms =
    bgroup (show n)
    [ env (cof n m) $
     \xs ->
          bgroup
              (show m)
              [ bench "const" $ nf constTrav xs
              , bench "id" $ nf identityTrav xs
              , bench "full" $ nf fullTrav xs]
    | m <- ms ]

main :: IO ()
main = defaultMain (map (uncurry benchAtSize) [(10000,[5,10,50])])
