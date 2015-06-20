{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

import TwitterMarkov.IO
import TwitterMarkov.TweetMarkov
import TwitterMarkov.MarkovModel
import qualified Data.Text.Lazy as T

import Control.Monad.State
import Control.Monad.Except
import System.Random
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  t <- evalStateT (runExceptT (generateTweet (args !! 0) (T.pack (args !! 1)))) (mkStdGen 1)
  print t

generateTweet :: FilePath -> T.Text -> ExceptT ParseError (StateT StdGen IO) [T.Text]
generateTweet fp seed = do
  tweets <- parseDir fp
  generate (tweetsModel tweets) seed
