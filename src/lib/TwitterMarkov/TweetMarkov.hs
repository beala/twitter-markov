{-# LANGUAGE OverloadedStrings #-}

module TwitterMarkov.TweetMarkov
(tweetsModel) where


import qualified Data.Text.Lazy            as T
import           TwitterMarkov.MarkovModel
import           TwitterMarkov.Types

tweetsModel :: [Tweet] -> MarkovModel T.Text
tweetsModel tweets = foldMap (uncurry singletonModel) (tweets >>= tweetNGram 2)

tweetNGram :: Int -> Tweet -> [(T.Text, T.Text)]
tweetNGram n t = nGram n . T.words . text $ t

nGram :: Int -> [T.Text] -> [(T.Text, T.Text)]
nGram n (a:as) = let (l, r) = splitAt n (takeExactly (n*2) (a:as)) in
                   if not (null l || null r)
                   then (T.unwords l, T.unwords r) : nGram n as
                   else []
nGram _ [] = []

takeExactly :: Int -> [a] -> [a]
takeExactly n as = if length (take n as) == n
                   then take n as
                   else []
