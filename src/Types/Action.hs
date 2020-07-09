module Types.Action where

import Types.Common

data Action
  = Join PlayerName
  | Move PlayerName Direction

data Direction
  = North
  | South
  | East
  | West
