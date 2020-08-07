module Logic.Update (update) where

import Data.Function ((&))
import qualified Data.List as List
import           Data.List.Unicode ((∈))
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
        Logout -> removeSession sessionID state
        Login user pass ->
          if Map.member sessionID (state & session)
          then sessionAlreadyInUse state
          else
            if user ∈ Map.elems (state & session)
            then userAlreadyLoggedIn state
            else
              case (state & players) !? user of
                Nothing ->
                  joinServerAndAddSession user pass sessionID state
                Just actualPass ->
                  if actualPass == pass
                  then addSession user sessionID state
                  else badPassword state
    Game ->
      case action of
        Login _ _ -> (state, NotImplemented)

badPassword :: State -> (State, Response)
badPassword state = (state, BadPassword)

addSession :: PlayerName -> SessionID -> State -> (State, Response)
addSession user sessionID state =
  ( state { session = Map.insert sessionID user (state & session) }
  , LoginSuccess
  )

joinServerAndAddSession :: PlayerName -> Password -> SessionID -> State -> (State, Response)
joinServerAndAddSession user pass sessionID state =
  ( state { players = Map.insert user pass (state & players)
          , session = Map.insert sessionID user (state & session)
          }
  , LobbyJoinSuccess
  )

userAlreadyLoggedIn :: State -> (State, Response)
userAlreadyLoggedIn state = (state, LoggedInFromDifferentSession)

sessionAlreadyInUse :: State -> (State, Response)
sessionAlreadyInUse state = (state, SessionAlreadyLoggedIn)

removeSession :: SessionID -> State -> (State, Response)
removeSession sessionID state =
  ( state { session = Map.delete sessionID (state & session) }
  , Disconnected
  )
