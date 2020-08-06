module Logic.Update (update) where

import Data.Function ((&))
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import           Data.Map (Map, (!?))
import qualified Data.Map as Map

import Common
import Constants
import Types.Common
import Types.Action
import Types.Response
import Types.State

update :: SessionID -> Action -> State -> (State, Response)
update sessionID action state =
  case state & mode of
    Lobby ->
      case action of
        Logout ->
          ( state { session = Map.delete sessionID (state & session) }
          , Disconnected)
        Login user pass ->
          case (state & session) !? sessionID of
            Just _ -> (state, SessionAlreadyLoggedIn)
            Nothing ->
              case List.find (==user) $ Map.elems (state & session) of
                Just _ -> (state, LoggedInFromDifferentSession)
                Nothing ->
                  case (state & players) !? user of
                    Nothing ->
                      ( state { players = Map.insert user pass (state & players)
                              , session = Map.insert sessionID user (state & session)
                              }
                      , LobbyJoinSuccess
                      )
                    Just actualPass ->
                      if actualPass == pass
                      then
                        ( state { session = Map.insert sessionID user (state & session) }
                        , LoginSuccess
                        )
                      else (state, BadPassword)
    Game ->
      case action of
        Login _ _ -> (state, NotImplemented)
