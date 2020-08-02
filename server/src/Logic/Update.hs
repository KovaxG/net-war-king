module Logic.Update (update) where

import Data.Function
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import Common
import Constants
import Types.Common
import Types.Action
import Types.Response
import Types.State

update :: Action -> State -> (State, Response)
update a state = case a of
  Join name -> joining name state
  Move name dir -> moving name dir state

joining :: PlayerName -> State -> (State, Response)
joining name state@(State players) =
  Map.lookup name players
  & fmap (\pd -> state --> Welcome (Info $ toClientData name pd players))
  & fromMaybe (
    State (Map.insert name newPlayerData players)
    --> Welcome (Info $ toClientData name newPlayerData players)
    )
  where
    newPlayerData = PlayerData initialSpawn

moving :: PlayerName -> Direction -> State -> (State, Response)
moving name dir state@(State players) =
  Map.lookup name players
  & fmap (\pd ->
    State (Map.insert name (move dir pd) players)
    --> Moved (Info $ toClientData name pd players)
  )
  & fromMaybe (state --> NonExistentPlayer)

move :: Direction -> PlayerData -> PlayerData
move dir (PlayerData (x,y)) =
  PlayerData $ case dir of
    North -> (x, y-playerSpeed)
    South -> (x, y+playerSpeed)
    East -> (x+playerSpeed, y)
    West -> (x-playerSpeed, y)
