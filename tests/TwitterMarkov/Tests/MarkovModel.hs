{-# LANGUAGE ScopedTypeVariables #-}

module TwitterMarkov.Tests.MarkovModel
(tests) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import TwitterMarkov.MarkovModel
import qualified Data.Map as Map
import Data.Monoid (Sum(..))

tests = testGroup "MarkovModel" [markovProps]

markovProps = testGroup "Markov model forms a monoid"
  [ QC.testProperty "empty model is neutral left" $
      \(model :: MarkovModel String) -> emptyModel `mappend` model == model
  , QC.testProperty "empty model is neutral right" $
      \(model :: MarkovModel String) -> model `mappend` emptyModel == model
  , QC.testProperty "mappend is associative" $
      \(model1 :: MarkovModel String) model2 model3 -> (model1 `mappend` model2) `mappend` model3 == model1 `mappend` (model2 `mappend` model3)
  ]

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (MonoidalValueMap k v) where
  arbitrary = do
    pairs <- listOf arbitrary
    return $ MonoidalValueMap (Map.fromList pairs)

instance (Arbitrary i) => Arbitrary (Sum i) where
  arbitrary = Sum <$> arbitrary