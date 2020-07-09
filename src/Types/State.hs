module Types.State where

import Data.Map (Map)
import qualified Data.Map as Map

import Types.Common

data State = State (Map PlayerName PlayerData) deriving (Show, Eq)

data PlayerData = PlayerData Speed Coord deriving (Show, Eq)
