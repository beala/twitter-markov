--module TwitterMarkov.MarkovModel.Tests
--(main, tests) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import TwitterMarkov.MarkovModel (MonoidalValueMap(..), emptyModel, MarkovModel)
import qualified Data.Map as Map
import Data.Monoid (Sum(..))

main :: IO ()
main = defaultMain tests

tests = testGroup "MarkovModel" [markovProps]

markovProps = testGroup "Markov Model Properties"
  [ QC.testProperty "empty model is neutral" $
      \model -> (emptyModel :: MarkovModel String) `mappend` model == model
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