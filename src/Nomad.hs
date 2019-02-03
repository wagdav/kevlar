module Nomad
  ( mkJob
  , Job
  , writeJob
  )
where

import           Control.Lens
import           Nomad.Aeson
import           Nomad.Job

import           Data.Aeson                     ( encode )
import qualified Data.ByteString.Lazy          as B

mkJob name image command volumes workDir sources =
  job
    &  jobID .~ name
    &  jobName .~ name
    &  jobType .~ "batch"
    &  jobTaskGroups
    .~ [ taskGroup
         &  taskGroupName .~ name
         &  taskGroupTasks
         .~ [ task
            & taskName      .~ name
            & taskDriver    .~ "docker"
            & taskArtifacts .~ artifacts
            & taskConfig . taskConfigImage .~ image
            & taskConfig . taskConfigCommand .~ command
            & taskConfig . taskConfigVolumes .~ volumes
            & taskConfig . taskConfigWorkDir .~ workDir
            ]
       ]
 where
  artifacts = [ Artifact s d | (s, d) <- sources ]

writeJob :: FilePath -> Job -> IO ()
writeJob p j = B.writeFile p (encode $ Nomad j)
