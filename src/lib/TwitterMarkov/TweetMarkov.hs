{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module TwitterMarkov.TweetMarkov
(tweetsModel) where

import qualified Data.Text.Lazy            as T

import           TwitterMarkov.MarkovModel
import           TwitterMarkov.Types

tweetsModel :: [Tweet] -> MarkovModel T.Text
tweetsModel tweets = foldMap (uncurry singletonModel) (tweets >>= twoGram)

twoGram :: Tweet -> [(T.Text, T.Text)]
twoGram t = zip tweetWords (drop 1 tweetWords)
  where tweetWords = T.words . text $ t
