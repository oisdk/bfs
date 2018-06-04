{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Hedgehog
import qualified Hedgehog.Gen                       as Gen
import qualified Hedgehog.Internal.Gen              as Gen
import qualified Hedgehog.Internal.Tree             as HTree
import qualified Hedgehog.Range                     as Range

import           Data.Functor.Identity

import           Control.Monad.Trans.Maybe

import           Data.Tree                          (Tree (..))
import qualified Data.Tree                          as Tree

import qualified Data.Tree.BreadthFirst.Applicative as Applicative
import qualified Data.Tree.BreadthFirst.Iterative   as Iterative
import qualified Data.Tree.BreadthFirst.Queued      as Queued
import qualified Data.Tree.BreadthFirst.Zippy       as Zippy

genTree
    :: Gen a -> Gen (Tree a)
genTree = Gen.GenT . (fmap . fmap) f . Gen.unGen
  where
    f = HTree.Tree . fmap h . HTree.runTree
    h nd@(HTree.Node _ xs) = HTree.Node (toTree nd) (fmap f xs)
    toTree (HTree.Node x xs) =
        Node x
        [ toTree y
        | HTree.Tree (MaybeT (Identity (Just y))) <- xs ]

prop_travidentity :: Property
prop_travidentity = property $ do
    xs <- forAll (genTree (Gen.int (Range.linear 0 15)))
    (runIdentity . Iterative.breadthFirst Identity) xs === xs
    (runIdentity . Queued.breadthFirst Identity) xs === xs
    (runIdentity . Zippy.breadthFirst Identity) xs === xs
    (runIdentity . Applicative.breadthFirst Identity) xs === xs
    (runIdentity . Iterative.unfold (\(Node y ys) -> (Identity (y, ys)))) xs === xs

prop_travorder :: Property
prop_travorder = property $ do
    xs <- forAll (genTree (Gen.int (Range.linear 0 15)))
    fst (Iterative.breadthFirst (\x -> ([x],())) xs) === (concat . Tree.levels) xs
    fst (Queued.breadthFirst (\x -> ([x],())) xs) === (concat . Tree.levels) xs
    fst (Zippy.breadthFirst (\x -> ([x],())) xs) === (concat . Tree.levels) xs
    fst (Applicative.breadthFirst (\x -> ([x],())) xs) === (concat . Tree.levels) xs
    (fst . Iterative.unfold (\(Node y ys) -> ([y], (y, ys)))) xs === (concat . Tree.levels) xs

prop_levels :: Property
prop_levels = property $ do
    xs <- forAll (genTree (Gen.int (Range.linear 0 15)))
    let ys = Tree.levels xs
    ys === Iterative.levels xs
    ys === Queued.levels xs
    ys === Zippy.levels xs

main :: IO Bool
main = checkParallel $$(discover)
