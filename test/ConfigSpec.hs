{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module ConfigSpec where

import           Data.Map.Strict               as Map
import           Data.Yaml                     as Y
import           Data.ByteString                ( ByteString )
import           Text.RawString.QQ
import           Test.Hspec

import           Kevlar.Pipeline
import           Kevlar.Yaml

scriptYaml :: ByteString
scriptYaml = [r|
name: myscript
script: run.sh
need:
  - build
caches:
  - "/root/.stack"
|]

dockerImageYaml :: ByteString
dockerImageYaml = [r|
name: myimage
docker_image:
  context: src/docker/myimage
|]

environmentYaml :: ByteString
environmentYaml = [r|
name: env
environment:
  HELLO: hello
  WORLD: world
|]

sourceYaml :: ByteString
sourceYaml = [r|
name: src
source: http://example.com
|]

spec = describe "YAML parser" $ do
  it "should parse script" $ do
    script <- Y.decodeThrow scriptYaml
    script `shouldBe` Script "myscript" "run.sh" ["/root/.stack"] ["build"]

  it "should parse docker_image" $ do
    script <- Y.decodeThrow dockerImageYaml
    script `shouldBe` DockerImage "myimage" "src/docker/myimage"

  it "should parse environment" $ do
    script <- Y.decodeThrow environmentYaml
    script `shouldBe` Environment
      "env"
      (Map.fromList [("HELLO", "hello"), ("WORLD", "world")])

  it "should parse source" $ do
    script <- Y.decodeThrow sourceYaml
    script `shouldBe` Source "src" "http://example.com"
