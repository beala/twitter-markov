module TwitterMarkov.MarkovModel
( emptyModel
, singletonModel
, generate
, generateRandom
, MarkovModel
, MonoidalValueMap(..)
, weightedRandom
, lookupTransitions
) where

import           Control.Monad.State
import           Data.List.NonEmpty  (NonEmpty (..))
import qualified Data.List.NonEmpty  as NE
import qualified Data.Map.Strict     as Map
import           Data.Monoid
import           System.Random


type MarkovModel a = MonoidalValueMap a (MonoidalValueMap a (Sum Int))

-- Map whose value is also a monoid.
newtype MonoidalValueMap k v = MonoidalValueMap (Map.Map k v) deriving (Show, Eq, Ord)

instance (Monoid v, Ord k) => Monoid (MonoidalValueMap k v) where
  mempty = MonoidalValueMap Map.empty
  (MonoidalValueMap l) `mappend` (MonoidalValueMap r) = MonoidalValueMap $ Map.unionWith mappend l r

-- A model containing no transitions
emptyModel :: (Ord a) => MarkovModel a
emptyModel = MonoidalValueMap Map.empty

-- Create model containing a single transition.
singletonModel :: (Ord a) => a -> a -> MarkovModel a
singletonModel first second = MonoidalValueMap (Map.singleton first (MonoidalValueMap (Map.singleton second 1)))

lookupTransitions :: (Ord a) => a -> MarkovModel a -> [(Sum Int, a)]
lookupTransitions k (MonoidalValueMap m) = maybe [] mapToWeightEdgeTuples (Map.lookup k m)
  where mapToWeightEdgeTuples (MonoidalValueMap inner) = fmap (\(s, w) -> (w, s)) (Map.toList inner)

generateRandom :: (Ord a, RandomGen s, MonadState s m) => MarkovModel a -> m [a]
generateRandom m = do
  seed <- randomStartState m
  generated <- generate m seed
  return $ seed : generated

generate :: (Ord a, RandomGen s, MonadState s m) => MarkovModel a -> a -> m [a]
generate m seed =
  case lookupTransitions seed m of
    [] -> return []
    edges -> do
      x <- weightedRandom $ NE.fromList edges
      xs <- generate m x
      return $ x : xs

randomStartState :: (RandomGen s, MonadState s m) => MarkovModel a -> m a
randomStartState (MonoidalValueMap modelMap) = do
  let keys = Map.keys modelMap
  r <- state $ randomR (0, length keys - 1)
  return $ keys !! r

weightedRandom :: (RandomGen s, MonadState s m) => NE.NonEmpty (Sum Int, b) -> m b
weightedRandom weights = do
  r <- state $ randomR (1, totalWeight)
  case weights of
    ((_, b) :| []) -> return b
    ((Sum w, b) :| ws) ->
      if r <= w then
        return b
      else
        weightedRandom (NE.fromList ws)
  where
    totalWeight = getSum $ foldMap fst weights
