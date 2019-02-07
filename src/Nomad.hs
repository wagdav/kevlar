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
            & taskArtifacts .~ [ Artifact s d | (s, d) <- sources ]
            & dockerImage   .~ image
            & dockerCommand .~ command
            & dockerVolumes .~ volumes
            & dockerWorkDir .~ workDir
            ]
       ]

writeJob :: FilePath -> Job -> IO ()
writeJob p j = B.writeFile p (encode $ Nomad j)
