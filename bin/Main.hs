{-# LANGUAGE ApplicativeDo #-}

import Kevlar
import qualified Kevlar.Git as Git
import Kevlar.Kaniko as Kaniko
import System.FilePath ((</>))
import System.Process (callProcess)

kevlarRepo = Git.WorkingCopy "/home/dwagner/projects/haskell/kevlar"

bootstrap repo = do
  kevlarSrc <- clone kevlarRepo
  img <- Kaniko.build "kevlar-builder" "docker/kevlar-builder" [Need kevlarSrc ""]
  src <- clone repo
  shell
    [ "cp ./repo/.kevlar/config.hs ./.kevlar/config.hs",
      "./ci/build.sh :kevlar-config"
    ]
    [ Need kevlarSrc "",
      Need src "repo",
      Image img,
      Cache ".stack",
      Environment [("KEVLAR_OUTPUT", "output")]
    ]

main :: IO ()
main = kevlarMainWith bootstrap runPipeline
  where
    runPipeline (Git.WorkingCopy path) (HostDir outputPath) = callProcess (outputPath </> "kevlar-config") [path]
    runPipeline (Git.Url url revision) (HostDir outputPath) = callProcess (outputPath </> "kevlar-config") [url, revision]
