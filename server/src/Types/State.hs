module Types.State where

import Data.Function

import Data.Map (Map)
import qualified Data.Map as Map

import Types.Common

data State = State
  { mode :: GameMode
  , players :: Map PlayerName PlayerData
  } deriving (Show, Eq)

data GameMode = Lobby | Game deriving (Show, Eq)

data PlayerData = PlayerData
  { password :: Password
  , ready :: Bool
  , session :: Maybe SessionID
  } deriving (Show, Eq)

initialState :: State
initialState = State
  { mode = Lobby
  , players = Map.empty
  }

isLobby :: State -> Bool
isLobby state = (state & mode) == Lobby
