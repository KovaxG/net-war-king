module Types.Action where

import Types.Common

data Action
  = Join PlayerName Password
  deriving (Read, Show)