{-# LANGUAGE ScopedTypeVariables #-}

module TwitterMarkov.Tests.MarkovModel
(tests) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import TwitterMarkov.MarkovModel (MonoidalValueMap(..), emptyModel, MarkovModel)
import qualified Data.Map as Map
import Data.Monoid (Sum(..))

tests = testGroup "MarkovModel" [markovProps]

markovProps = testGroup "Markov model forms a monoid"
  [ QC.testProperty "empty model is neutral left" $
      \model -> (emptyModel :: MarkovModel String) `mappend` model == model
  , QC.testProperty "empty model is neutral right" $
      \model -> model `mappend` (emptyModel :: MarkovModel String) == model
  , QC.testProperty "mappend is associative" $
      \(model1 :: MarkovModel String) model2 model3 -> (model1 `mappend` model2) `mappend` model3 == model1 `mappend` (model2 `mappend` model3)
  ]

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (MonoidalValueMap k v) where
  arbitrary = do
    pairs <- listOf tup
    return $ MonoidalValueMap (Map.fromList pairs)
    where
      tup = do
        k <- arbitrary
        v <- arbitrary
        return (k,v)

instance (Arbitrary i) => Arbitrary (Sum i) where
  arbitrary = fmap Sum (arbitrary)