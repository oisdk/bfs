{-# options_ghc -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}

module Main (main) where

import           Control.Applicative
import           Control.Monad
import           Criterion.Main
import           Data.Bifunctor
import           Data.Functor.Identity
import           Data.Monoid
import           Data.Tree
import           System.Random

import qualified Data.Tree.BreadthFirst.Iterative as Iterative
import qualified Data.Tree.BreadthFirst.Queued    as Queued
import qualified Data.Tree.BreadthFirst.Zippy     as Zippy

import           Control.Lens


int :: Int -> IO Int
int n = randomRIO (1,n)

tree :: Int -> Int -> IO (Tree Int)
tree n m = go n
  where
    go i | i <= 1 = flip Node [] <$> int n
         | otherwise = liftA2 Node (int n) (replicateM m (go (i `div` m)))

constTrav :: Traversal' (Tree Int) Int -> Tree Int -> Int
constTrav bf = getSum . getConst . bf (Const . Sum)

identityTrav :: Traversal' (Tree a) a -> Tree a -> Tree a
identityTrav bf = runIdentity . bf Identity

fullTrav :: Traversal' (Tree Int) Int -> Tree Int -> (Int, Tree Int)
fullTrav bf = first getSum . bf (\x -> (Sum x, x+1))

-- expTrav :: Traversal' (Tree Int) Int -> Tree Int -> [Tree Int]
-- expTrav bf = bf (\x -> [x, x])

benchAtSize :: Int -> [Int] -> Benchmark
benchAtSize n ms =
    bgroup
        (show n)
        [ env (tree n m) $
         \xs ->
              bgroup
                  (show m)
                  [ bgroup
                        "traversals"
                        [ bgroup
                              "const"
                              [ bench "iterative" $
                                nf (constTrav Iterative.breadthFirst) xs
                              , bench "queued" $
                                nf (constTrav Queued.breadthFirst) xs
                              , bench "zippy" $
                                nf (constTrav Zippy.breadthFirst) xs]
                        , bgroup
                              "id"
                              [ bench "iterative" $
                                nf (identityTrav Iterative.breadthFirst) xs
                              , bench "queued" $
                                nf (identityTrav Queued.breadthFirst) xs
                              , bench "zippy" $
                                nf (identityTrav Zippy.breadthFirst) xs]
                        , bgroup
                              "full"
                              [ bench "iterative" $
                                nf (fullTrav Iterative.breadthFirst) xs
                              , bench "queued" $
                                nf (fullTrav Queued.breadthFirst) xs
                              , bench "zippy" $
                                nf (fullTrav Zippy.breadthFirst) xs]]
                  , bgroup
                        "levels"
                        [ bench "iterative" $ nf Iterative.levels xs
                        , bench "queued" $ nf Queued.levels xs
                        , bench "zippy" $ nf Zippy.levels xs]]
        | m <- ms ]

main :: IO ()
main = defaultMain (map (uncurry benchAtSize) [(10000,[5,50,100])])
