{-# LANGUAGE ScopedTypeVariables #-}

module TwitterMarkov.Tests.MarkovModel
(tests) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import TwitterMarkov.MarkovModel
import qualified Data.Map as Map
import Data.Monoid (Sum(..))
import qualified Data.List.NonEmpty as NE
import Control.Monad.State
import System.Random

tests = testGroup "MarkovModel" [markovProps, randomProps]

markovProps = testGroup "Markov model forms a monoid"
  [ QC.testProperty "empty model is neutral left" $
      \(model :: MarkovModel String) -> emptyModel `mappend` model == model
  , QC.testProperty "empty model is neutral right" $
      \(model :: MarkovModel String) -> model `mappend` emptyModel == model
  , QC.testProperty "mappend is associative" $
      \(model1 :: MarkovModel String) model2 model3 -> (model1 `mappend` model2) `mappend` model3 == model1 `mappend` (model2 `mappend` model3)
  ]

randomProps = testGroup "Randomness properties"
  [ QC.testProperty "Weight zero is never chosen" $
    \(randomWeights :: NE.NonEmpty (Sum Int, String)) (seed :: Int) ->
     let weights = (Sum 0, "never chosen") NE.<| randomWeights
          r = weightedRandom weights
      in
        evalState r (mkStdGen seed) /= "never chosen"
  ]

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (MonoidalValueMap k v) where
  arbitrary = do
    pairs <- listOf arbitrary
    return $ MonoidalValueMap (Map.fromList pairs)

instance (Arbitrary a) => Arbitrary (NE.NonEmpty a) where
  arbitrary = do
    xs <- listOf1 arbitrary
    return $ NE.fromList xs

instance (Arbitrary i) => Arbitrary (Sum i) where
  arbitrary = Sum <$> arbitrary
