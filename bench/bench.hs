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

import qualified Data.Tree.BreadthFirst.Applicative as Applicative
import qualified Data.Tree.BreadthFirst.Iterative   as Iterative
import qualified Data.Tree.BreadthFirst.Queued      as Queued
import qualified Data.Tree.BreadthFirst.Zippy       as Zippy

int :: Int -> IO Int
int n = randomRIO (1,n)

tree :: Int -> Int -> IO (Tree Int)
tree n m = go n
  where
    go i | i <= 1 = flip Node [] <$> int n
         | otherwise = liftA2 Node (int n) (replicateM m (go (i `div` m)))

constTrav
    :: ((Int -> Const (Sum Int) Int) -> Tree Int -> Const (Sum Int) (Tree Int))
    -> Tree Int
    -> Int
constTrav bf = getSum . getConst . bf (Const . Sum)

identityTrav
    :: ((Int -> Identity Int) -> Tree Int -> Identity (Tree Int))
    -> Tree Int
    -> Tree Int
identityTrav bf = runIdentity . bf Identity

fullTrav
    :: ((Int -> (Sum Int, Int)) -> Tree Int -> (Sum Int, Tree Int))
    -> Tree Int
    -> (Int, Tree Int)
fullTrav bf = first getSum . bf (\x -> (Sum x, x+1))

-- expTrav :: Traversal' (Tree Int) Int -> Tree Int -> [Tree Int]
-- expTrav bf = bf (\x -> [x, x])

traversals :: Applicative f => [(String, (a -> f b) -> Tree a -> f (Tree b))]
traversals =
    [ ("iterative", Iterative.breadthFirst)
    , ("queued", Queued.breadthFirst)
    , ("zippy", Zippy.breadthFirst)
    , ("applicative", Applicative.breadthFirst)]


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
                              [ bench nm $ nf (constTrav trav) xs
                              | (nm,trav) <- traversals ]
                        , bgroup
                              "id"
                              [ bench nm $ nf (identityTrav trav) xs
                              | (nm,trav) <- traversals ]
                        , bgroup
                              "full"
                              [ bench nm $ nf (fullTrav trav) xs
                              | (nm,trav) <- traversals ]]
                  , bgroup
                        "levels"
                        [ bench "iterative" $ nf Iterative.levels xs
                        , bench "queued" $ nf Queued.levels xs
                        , bench "zippy" $ nf Zippy.levels xs]]
        | m <- ms ]

main :: IO ()
main = defaultMain (map (uncurry benchAtSize) [(10000,[5,50,100])])
