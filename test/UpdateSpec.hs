module UpdateSpec (spec) where

import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Test.Hspec

import Common
import Constants
import Logic.Update
import TestUtils
import Types.Action
import Types.Response
import Types.State

spec :: Spec
spec = do
  describe "update function" $ do
    it "a new player joining is added to the state" $ do
      let name = "Kovax"
      let action = Join name
      let state = State Map.empty
      let (newState, response) = update action state

      sequence_
        [ newState === State (Map.fromList [name --> PlayerData defaultSpeed initialSpawn])
        , response === Welcome (Info [])
        ]

    it "an old player joining is not teleported" $ do
      let name = "Kovax"
      let position = (42,42)
      let action = Join name
      let state = State (Map.fromList [name --> PlayerData defaultSpeed position])
      let (newState, response) = update action state

      sequence_
        [ newState === State (Map.fromList [name --> PlayerData defaultSpeed position])
        , response === Welcome (Info [])
        ]

    it "moving a non existent player should have no effect" $ do
      let state = State Map.empty
      let action = Move "Kovax" East
      let (newState, response) = update action state

      sequence_
        [ newState === State Map.empty
        , response === NonExistentPlayer
        ]

    it "moving a player should have the desired effect" $ do
      let name = "Kovax"
      let action = Move name East
      let state = State (Map.fromList [name --> PlayerData defaultSpeed (42,42)])
      let (newState, response) = update action state

      sequence_
        [ newState === State (Map.fromList [name --> PlayerData defaultSpeed (42 + defaultSpeed,42)])
        , response === Moved (Info [])
        ]

    it "spawning next to a player makes that player visible" $ do
      let newPlayer = "Kovax"
      let oldPlayer = "Loosner"
      let action = Join newPlayer
      let state = State (Map.fromList [oldPlayer --> PlayerData defaultSpeed (4,4)])
      let (newState, response) = update action state

      sequence_
        [ newState === State (Map.fromList [ newPlayer --> PlayerData defaultSpeed (0,0)
                                           , oldPlayer --> PlayerData defaultSpeed (4,4)])
        , response === Welcome (Info [PlayerClientData "Loosner" (4,4)])
        ]
