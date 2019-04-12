module Kevlar.Git
  ( copyGitFiles
  ) where

import Control.Monad (forM_)
import qualified Data.Set as S
import System.Directory
import System.FilePath
import System.Process (readProcess)

copyGitFiles :: FilePath -> IO ()
copyGitFiles dstDir = do
  files <- lsFiles
  forM_ files copy
  where
    copy :: FilePath -> IO ()
    copy f = do
      createDirectoryIfMissing True (dstDir </> takeDirectory f)
      copyFile f (dstDir </> f)

lsFiles :: IO [FilePath]
lsFiles = do
  tracked <- gitLs []
  deleted <- gitLs ["--deleted"]
  others <- gitLs ["--others", "--exclude-standard"]
  return $ S.toList $ (tracked `S.difference` deleted) `S.union` others

gitLs :: [String] -> IO (S.Set FilePath)
gitLs args = (S.fromList . lines) <$> readProcess "git" ("ls-files" : args) ""
