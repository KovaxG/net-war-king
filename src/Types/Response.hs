module Types.Response where

import Data.Function
import Data.Map (Map)
import qualified Data.Map as Map

import Common
import Constants
import Types.Common
import Types.State

data Response
  = Welcome Info -- sent when a new player joins the server
  | Moved Info -- sent after a move
  | NonExistentPlayer -- sent whenever a player is not recognized
  deriving (Show, Eq)

data Info = Info [PlayerClientData] deriving (Show, Eq)

data PlayerClientData = PlayerClientData PlayerName Coord deriving (Show, Eq)

toClientData :: PlayerName -> PlayerData -> Map PlayerName PlayerData -> [PlayerClientData]
toClientData name0 (PlayerData _ p0) players =
  players
  & Map.toList
  & filter (\(name, PlayerData _ p) -> manDist p0 p < defaultViewDistance && name /= name0)
  & fmap (\(name, PlayerData _ p) -> PlayerClientData name (relativeTo p0 p))
