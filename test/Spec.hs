{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Hedgehog
import qualified Hedgehog.Gen                 as Gen
import qualified Hedgehog.Internal.Gen        as Gen
import qualified Hedgehog.Internal.Tree       as HTree
import qualified Hedgehog.Range               as Range

import           Control.Comonad.Cofree.BreadthFirst

import           Control.Comonad.Cofree
import           Control.Lens                 hiding ((:<))
import           Control.Monad.Trans.Maybe
import           Control.Monad ((<=<))

import           Data.Tree                    (Tree (..))
import qualified Data.Tree                    as Tree

tree :: Iso (Cofree [] a) (Cofree [] b)(Tree a) (Tree b)
tree = iso toTree fromTree
  where
    toTree (x :< xs) = Node x (map toTree xs)
    fromTree (Node x xs) = x :< map fromTree xs

genCofree
    :: Gen a -> Gen (Cofree [] a)
genCofree = Gen.GenT . (fmap . fmap) f . Gen.unGen
  where
    f = HTree.Tree . fmap h . HTree.runTree
    h nd@(HTree.Node _ xs) = HTree.Node (toCofree nd) (fmap f xs)
    toCofree (HTree.Node x xs) =
        x :<
        [ toCofree y
        | HTree.Tree (MaybeT (Identity (Just y))) <- xs ]

prop_travidentity :: Property
prop_travidentity = property $ do
    xs <- forAll (genCofree (Gen.int (Range.linear 0 15)))
    (runIdentity . breadthFirst Identity) xs === xs

prop_travorder :: Property
prop_travorder = property $ do
    xs <- forAll (genCofree (Gen.int (Range.linear 0 15)))
    fst (breadthFirst (\x -> ([x],())) xs) === views tree (concat . Tree.levels) xs

prop_itravorder :: Property
prop_itravorder = property $ do
    xs <- forAll (genCofree (Gen.int (Range.linear 0 15)))
    fst (ibreadthFirst (\i x ->([(i,x)], ())) xs) === views tree (sequenceA <=< zip [0..] . Tree.levels) xs


main :: IO Bool
main = checkParallel $$(discover)
