{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module TwitterMarkov.Types
( Tweet(..),
  text
) where

import           Control.Applicative
import           Control.Monad       (mzero)
import           Data.Aeson
import           Data.Text.Lazy
import           Data.Typeable
import           GHC.Generics

data Tweet = Tweet {text :: Text} deriving (Show, Eq, Ord, Generic, Typeable)

instance FromJSON Tweet where
  parseJSON (Object v) = Tweet <$> v .: "text"
  parseJSON _ = mzero
