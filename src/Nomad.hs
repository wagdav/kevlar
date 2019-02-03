module Nomad
  ( mkJob
  , Job
  , writeJob
  )
where

import Nomad.Aeson
import Nomad.Job

import           Data.Aeson                     ( encode )
import qualified Data.ByteString.Lazy          as B

mkJob name image command volumes workDir sources = Job
  name  -- id
  name  -- name
  "batch"
  ["dc1"]
  [TaskGroup name 1 [task]]
  (ReschedulePolicy 0)
 where
  task =
    Task name "docker" (TaskConfig image command volumes workDir) artifacts
  artifacts = [ Artifact s d | (s, d) <- sources ]

writeJob :: FilePath -> Job -> IO ()
writeJob p j = B.writeFile p (encode $ Nomad j)

defaultJob = Job "" "" "" [] [] (ReschedulePolicy 0)
