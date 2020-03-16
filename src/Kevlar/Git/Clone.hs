module Kevlar.Git.Clone where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Kevlar.Git.Types
import System.Directory (copyFile, createDirectoryIfMissing)
import System.FilePath
import System.Process (callProcess, readProcess)

-- | Clone a Git repository to the specified local directory
clone :: Repository -> FilePath -> IO ()
clone repo dest = do
  createDirectoryIfMissing True dest
  case repo of
    WorkingCopy src -> copyFiles src dest
    Url url revision -> do
      callProcess "git" ["clone", url, dest]
      callProcess "git" ["-C", dest, "reset", "--hard", revision]

-- | Copy a Git source tree from 'srcDir' to 'dstDir'
copyFiles :: FilePath -> FilePath -> IO ()
copyFiles srcDir dstDir = mapM_ copy =<< lsFiles srcDir
  where
    copy :: FilePath -> IO ()
    copy f = do
      createDirectoryIfMissing True (dstDir </> takeDirectory f)
      copyFile (srcDir </> f) (dstDir </> f)

lsFiles :: FilePath -> IO [FilePath]
lsFiles repo = do
  tracked <- gitLs repo []
  deleted <- gitLs repo ["--deleted"]
  others <- gitLs repo ["--others", "--exclude-standard"]
  return $ Set.toList $ (tracked `Set.difference` deleted) `Set.union` others

gitLs :: FilePath -> [String] -> IO (Set FilePath)
gitLs repo args =
  Set.fromList . lines
    <$> readProcess "git" (["-C", repo, "ls-files"] ++ args) ""
