{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module TwitterMarkov.IO
( parseFile
, parseDir
, ParseError(..)
) where
import Control.Applicative
import Data.Traversable
import           System.Directory

import           Control.Monad.Except
import           Data.Aeson
import           Data.List                  (isSuffixOf)

import qualified Data.ByteString.Lazy.Char8 as C

import qualified Data.Text.Lazy             as T
import qualified Data.Text.Lazy.Encoding    as TE (encodeUtf8)
import qualified Data.Text.Lazy.IO          as TIO (readFile)

import           Safe

import           TwitterMarkov.Types

data ParseError = ParseError T.Text T.Text deriving (Show)

parseFile :: (MonadIO m, MonadError ParseError m) => FilePath -> m [Tweet]
parseFile filePath = do
  rawContents <- liftIO $ TIO.readFile filePath
  case eitherDecode . preprocess $ rawContents of
    Left e -> throwError $ ParseError (T.pack filePath) (T.pack e)
    Right p -> return p

preprocess :: T.Text -> C.ByteString
preprocess = TE.encodeUtf8 . T.unlines . tailSafe . T.lines

parseDir :: (Functor m, Applicative m, MonadIO m, MonadError ParseError m) => FilePath -> m [Tweet]
parseDir dirPath = do
  files <- liftIO $ getDirectoryFiles dirPath
  join <$> traverse parseFile files

getDirectoryFiles :: FilePath -> IO [FilePath]
getDirectoryFiles p = do
  allContents <- getDirectoryContents p
  let absContents = makeAbsPath allContents
  files <- filterM doesFileExist absContents
  return $ filter (isSuffixOf ".js") files

  where
    makeAbsPath paths = ((p ++ "/") ++) <$> paths
