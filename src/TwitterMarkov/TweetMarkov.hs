{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

module TwitterMarkov.TweetMarkov
(tweetsModel) where

import qualified Data.Text.Lazy as T

import           TwitterMarkov.Types
import           TwitterMarkov.MarkovModel

tweetModel :: Tweet -> MarkovModel T.Text
tweetModel t = foldMap (uncurry singletonModel) nGrams
  where nGrams = twoGram t

tweetsModel :: [Tweet] -> MarkovModel T.Text
tweetsModel = foldMap tweetModel

twoGram :: Tweet -> [(T.Text, T.Text)]
twoGram t = zip tweetWords (drop 1 tweetWords)
  where tweetWords = T.words . text $ t
