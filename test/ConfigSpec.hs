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
need: ["src"]
|]

paramsYaml :: ByteString
paramsYaml = [r|
name: env
params:
  HELLO: hello
  WORLD: world
|]

sourceYaml :: ByteString
sourceYaml = [r|
name: src
source: http://example.com
|]

secretsYaml :: ByteString
secretsYaml = [r|
name: mysecrets
secrets:
  API_KEY: /api/deploy_key
|]

spec = describe "YAML parser" $ do
  it "should parse script" $ do
    script <- Y.decodeThrow scriptYaml
    script `shouldBe` Script "myscript" "run.sh" ["/root/.stack"] ["build"]

  it "should parse docker_image" $ do
    script <- Y.decodeThrow dockerImageYaml
    script `shouldBe` DockerImage "myimage" "src/docker/myimage" ["src"]

  it "should parse params" $ do
    script <- Y.decodeThrow paramsYaml
    script `shouldBe` Params
      "env"
      (Map.fromList [("HELLO", "hello"), ("WORLD", "world")])

  it "should parse source" $ do
    script <- Y.decodeThrow sourceYaml
    script `shouldBe` Source "src" "http://example.com"

  it "should parse secrets block" $ do
    script <- Y.decodeThrow secretsYaml
    script `shouldBe` Secrets "mysecrets"
      (Map.fromList [("API_KEY", "/api/deploy_key")])
