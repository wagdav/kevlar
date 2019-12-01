{-# LANGUAGE ApplicativeDo #-}

import Data.Version (showVersion)
import Kevlar
import Kevlar.Kaniko as Kaniko
import Paths_kevlar (version)

-- Kevlar's own pipeline
buildKevlar = do
  repo <- clone "."
  img <- Kaniko.build "kevlar-builder" "docker/kevlar-builder" [Need repo ""]
  shell
    ["./ci/build.sh"]
    [ Need repo "",
      Image "kevlar-builder",
      Load img,
      Cache ".stack",
      Environment [("KEVLAR_OUTPUT", "output")]
    ]

publishKevlar binary = do
  repo <- clone "."
  img <- Kaniko.build "kevlar-publish" "docker/kevlar-publish" [Need repo ""]
  shell
    ["./ci/publish.sh"]
    [ Need binary "build",
      Need repo "",
      Image "kevlar-publish",
      Load img,
      Environment [("KEVLAR_VERSION", "v" <> showVersion version)],
      Secret "GITHUB_ACCESS_TOKEN"
    ]

buildAndPublish = buildKevlar >>= publishKevlar

main :: IO ()
main = kevlarMain buildKevlar
