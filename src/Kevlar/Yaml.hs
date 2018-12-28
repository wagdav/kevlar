{-# LANGUAGE OverloadedStrings #-}
module Kevlar.Yaml where

import           Data.Foldable                  ( asum )
import           Data.Aeson.Types
import qualified Data.Yaml                     as Y

import           Kevlar.Pipeline

instance FromJSON Pipeline where
  parseJSON = withObject "pipeline" $ \o ->
    Pipeline <$> o .: "artifacts"

instance FromJSON Step where
  parseJSON = withObject "step" $ \o -> asum
    [ Script <$> o .:  "name"
             <*> o .:  "script"
             <*> o .:? "caches" .!= []
             <*> o .:? "need"   .!= []

    , {- DockerImage -} do
        name    <- o .:  "name"
        need    <- o .:? "need" .!= []
        argsO   <- o .:  "docker_image"
        context <- argsO .:  "context"
        return $ DockerImage name context need

    , Params <$> o .: "name" <*> o .: "params"

    , Source <$> o .: "name" <*> o .: "source"

    , Secrets <$> o .: "name" <*> o .: "secrets"
    ]

readPipeline :: FilePath -> IO Pipeline
readPipeline = Y.decodeFileThrow
