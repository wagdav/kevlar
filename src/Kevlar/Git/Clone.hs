module Kevlar.Git.Clone where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Kevlar.Git.Types
import System.Directory
import System.FilePath
import System.Process (callProcess, readProcess)

-- | Clone a Git repository to the specified local directory
clone :: Repository -> FilePath -> IO ()
clone repo dest = do
  createDirectoryIfMissing True dest
  case repo of
    WorkingCopy src -> withCurrentDirectory src (copyFiles dest)
    Url url revision -> do
      callProcess "git" ["clone", url, dest]
      gitReset revision
  where
    gitReset x =
      withCurrentDirectory dest $
        callProcess "git" ["reset", "--hard", x]

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
