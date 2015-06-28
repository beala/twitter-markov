{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy            as T
import           TwitterMarkov.IO
import           TwitterMarkov.MarkovModel
import           TwitterMarkov.TweetMarkov


import           Control.Monad.Except
import           Control.Monad.State
import           System.Environment
import           System.Random

main :: IO ()
main = do
  args <- getArgs
  stdGen <- getStdGen
  t <- evalStateT (runExceptT (generateTweet (args !! 0))) stdGen
  print (T.unwords <$> t)

generateTweet :: FilePath -> ExceptT ParseError (StateT StdGen IO) [T.Text]
generateTweet fp = do
  tweets <- parseDir fp
  generateRandom (tweetsModel tweets)
