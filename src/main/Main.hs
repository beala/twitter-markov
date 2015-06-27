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
  t <- evalStateT (runExceptT (generateTweet (args !! 0) (T.pack (args !! 1)))) (mkStdGen 1)
  print t

generateTweet :: FilePath -> T.Text -> ExceptT ParseError (StateT StdGen IO) [T.Text]
generateTweet fp seed = do
  tweets <- parseDir fp
  generate (tweetsModel tweets) seed
