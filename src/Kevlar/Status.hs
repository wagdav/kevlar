module Kevlar.Status where

-- | Notification status
data Status = Error | Failure | Pending | Success
  deriving (Eq, Show)
