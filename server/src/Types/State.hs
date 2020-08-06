module Types.State where

import Data.Function

import Data.Map (Map)
import qualified Data.Map as Map

import Types.Common

data State = State
  { mode :: GameMode
  , players :: Map PlayerName Password
  , session :: Map SessionID PlayerName
  } deriving (Show, Eq)

data GameMode = Lobby | Game deriving (Show, Eq)

data PlayerData = PlayerData Coord deriving (Show, Eq)

initialState :: State
initialState = State
  { mode = Lobby
  , players = Map.empty
  , session = Map.empty
  }

isLobby :: State -> Bool
isLobby state = (state & mode) == Lobby
