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

expTrav :: Traversable f => Cofree f Int -> [Cofree f Int]
expTrav = breadthFirst (\x -> [x, x])

constTrav2 :: Traversable f =>  Cofree f Int -> Int
constTrav2 = getSum . getConst . breadthFirst2 (Const . Sum)

identityTrav2 :: Traversable f => Cofree f a -> Cofree f a
identityTrav2 = runIdentity . breadthFirst2 Identity

fullTrav2 :: Traversable f => Cofree f Int -> (Int, Cofree f Int)
fullTrav2 = first getSum . breadthFirst2 (\x -> (Sum x, x+1))

expTrav2 :: Traversable f => Cofree f Int -> [Cofree f Int]
expTrav2 = breadthFirst2 (\x -> [x, x])

benchAtSize :: Int -> [Int] -> Benchmark
benchAtSize n ms =
    bgroup
        (show n)
        [ env (cof n m) $
         \xs ->
              bgroup
                  (show m)
                  [ bgroup
                        "traversals"
                        [ bench "const/old" $ nf constTrav2 xs
                        , bench "const/new" $ nf constTrav xs
                        , bench "id/old"    $ nf identityTrav2 xs
                        , bench "id/new"    $ nf identityTrav xs
                        , bench "full/old"  $ nf fullTrav2 xs
                        , bench "full/new"  $ nf fullTrav xs
                        -- , bench "exp/old"   $ nf expTrav2 xs
                        -- , bench "exp/new"   $ nf expTrav xs
                        ]
                  , bgroup
                        "levels"
                        [ bench "levels1" $ nf levels1 xs
                        , bench "levels2" $ nf levels2 xs
                        , bench "levels3" $ nf levels3 xs]]
        | m <- ms ]

main :: IO ()
main = defaultMain (map (uncurry benchAtSize) [(50,[2,10,1000])])
