module Types.State where

import Data.Map (Map)
import qualified Data.Map as Map

import Types.Common

data State
  = State Int
  deriving (Show, Eq)

data PlayerData = PlayerData Coord deriving (Show, Eq)
