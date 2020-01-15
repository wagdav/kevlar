{-# LANGUAGE ApplicativeDo #-}

import Data.Version (showVersion)
import Kevlar
import Kevlar.Kaniko as Kaniko
import Paths_kevlar (version)

-- Kevlar's own pipeline
buildKevlar repo = do
  src <- clone repo
  img <- Kaniko.build "kevlar-builder" "docker/kevlar-builder" [Need src ""]
  shell
    ["./ci/build.sh"]
    [ Need src "",
      Image img,
      Cache ".stack",
      Environment [("KEVLAR_OUTPUT", "output")]
    ]

publishKevlar repo binary = do
  src <- clone repo
  img <- Kaniko.build "kevlar-publish" "docker/kevlar-publish" [Need src ""]
  shell
    ["./ci/publish.sh"]
    [ Need binary "build",
      Need src "",
      Image img,
      Environment [("KEVLAR_VERSION", "v" <> showVersion version)],
      Secret "GITHUB_ACCESS_TOKEN"
    ]

buildAndPublish src = buildKevlar src >>= publishKevlar src

main :: IO ()
main = kevlarMain buildKevlar
