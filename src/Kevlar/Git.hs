module Kevlar.Git
  ( copyFiles,
  )
where

import Data.Set (Set)
import qualified Data.Set as Set
import System.Directory
import System.FilePath
import System.Process (readProcess)

-- | Copy a Git source tree in the current directory to 'dstDir'
copyFiles :: FilePath -> IO ()
copyFiles dstDir = mapM_ copy =<< lsFiles
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
  return $ Set.toList $ (tracked `Set.difference` deleted) `Set.union` others

gitLs :: [String] -> IO (Set FilePath)
gitLs args = Set.fromList . lines <$> readProcess "git" ("ls-files" : args) ""
