module Kevlar.Volume where

data VolumeOption
  = ReadOnly
  | ReadWrite
  deriving (Eq, Show)

volumeOption :: VolumeOption -> String
volumeOption ReadOnly = "ro"
volumeOption ReadWrite = "rw"

newtype HostPath =
  HostPath FilePath

newtype ContainerPath =
  ContainerPath FilePath

volume :: HostPath -> ContainerPath -> VolumeOption -> [String]
volume (HostPath local) (ContainerPath remote) opt =
  ["--volume", local ++ ":" ++ remote ++ ":" ++ volumeOption opt]
