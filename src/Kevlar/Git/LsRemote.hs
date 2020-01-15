module Kevlar.Git.LsRemote where

import Data.Map (Map)
import qualified Data.Map as Map
import Kevlar.Git.Types
import System.Process (readProcess)

lsRemote :: String -> IO (Map String Revision)
lsRemote repo =
  Map.fromList
    . map (toRefHash . words)
    . lines
    <$> readProcess "git" ["ls-remote", repo] ""
  where
    toRefHash [hash, ref] = (ref, hash) -- Note: order is reversed!
