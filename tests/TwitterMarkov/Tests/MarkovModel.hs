{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

tests = testGroup "MarkovModel" [monoidProps, randomProps, modelProps]

monoidProps = testGroup "Markov model forms a monoid"
  [ QC.testProperty "empty model is neutral left" $
      \(model :: MarkovModel String) -> emptyModel `mappend` model == model
  , QC.testProperty "empty model is neutral right" $
      \(model :: MarkovModel String) -> model `mappend` emptyModel == model
  , QC.testProperty "mappend is associative" $
      \(model1 :: MarkovModel String) model2 model3 -> 
       (model1 `mappend` model2) `mappend` model3 == model1 `mappend` (model2 `mappend` model3)
  ]

modelProps = testGroup "Markov model properties"
  [ QC.testProperty "Markov transition weights sum" $
      \(startState :: String) (endState :: String) ->
       let transition = singletonModel startState endState
           sumModel = transition `mappend` transition
           lookedUp = lookupTransitions startState sumModel in
       length lookedUp == 1 &&
       fst (head lookedUp) == (Sum 2) &&
       snd (head lookedUp) == endState
  , QC.testProperty "Singleton model always takes single transition" $
      \(start :: String) (end :: String) (seed :: Int) ->
       let model = singletonModel start end
           r = generateRandom model
           result  = evalState r (mkStdGen seed) in
       (take 2 result) == [start, end]
  ]

toWeights :: NonEmptyList (Positive Int, String) -> NE.NonEmpty (Sum Int, String)
toWeights (NonEmpty l) = NE.fromList $ fmap toWeightTup l
  where toWeightTup (Positive i, s) = (Sum i, s)

randomProps = testGroup "Randomness properties"
  [ QC.testProperty "Weight zero is never chosen" $
      \(randWeights :: NonEmptyList (Positive Int, String)) (seed :: Int) ->
       let weights = (Sum 0, "never chosen") NE.<| (toWeights randWeights)
           r = weightedRandom weights
           result = evalState r (mkStdGen seed) in
       result /= "never chosen"
  , QC.testProperty "Weight one is always chosen if other weights are 0." $
      \(randStrings :: [String]) (seed :: Int) ->
      let weights = NE.fromList $ (Sum 1, "always chosen") : fmap ((,) (Sum 0)) randStrings
          r = weightedRandom weights
          result = evalState r (mkStdGen seed) in
      result == "always chosen"
  , QC.testProperty "Single option with positive weight is always chosen" $
      \(weight :: Positive Int) (randString :: String) (seed :: Int) ->
       let weights = NE.fromList [(Sum (getPositive weight), randString)]
           r = weightedRandom weights
           result = evalState r (mkStdGen seed) in
       result == randString
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
