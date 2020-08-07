module Logic.Update (update) where

import           Data.Function ((&))
import qualified Data.List as List
import           Data.Foldable.Unicode ((∈))
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
          loginFlow sessionID user pass state (joinServerAndAddSession user pass sessionID state)

    Game ->
      case action of
        Logout -> removeSession sessionID state
        Login user pass ->
          loginFlow sessionID user pass state (userNotPartOfServer state)

loginFlow :: SessionID -> PlayerName -> Password -> State -> (State, Response) -> (State, Response)
loginFlow sessionID user pass state newUserFlow =
  if Map.member sessionID (state & sessions)
  then sessionAlreadyInUse state
  else
    if user ∈ (state & sessions)
    then userAlreadyLoggedIn state
    else
      case (state & players) !? user of
        Nothing -> newUserFlow
        Just actualPass ->
          if actualPass == pass
          then addSession user sessionID state
          else badPassword state

badPassword :: State -> (State, Response)
badPassword state = (state, BadPassword)

addSession :: PlayerName -> SessionID -> State -> (State, Response)
addSession user sessionID state =
  ( state { sessions = Map.insert sessionID user (state & sessions) }
  , LoginSuccess
  )

joinServerAndAddSession :: PlayerName -> Password -> SessionID -> State -> (State, Response)
joinServerAndAddSession user pass sessionID state =
  ( state { players = Map.insert user pass (state & players)
          , sessions = Map.insert sessionID user (state & sessions)
          }
  , LobbyJoinSuccess
  )

userAlreadyLoggedIn :: State -> (State, Response)
userAlreadyLoggedIn state = (state, LoggedInFromDifferentSession)

sessionAlreadyInUse :: State -> (State, Response)
sessionAlreadyInUse state = (state, SessionAlreadyLoggedIn)

removeSession :: SessionID -> State -> (State, Response)
removeSession sessionID state =
  ( state { sessions = Map.delete sessionID (state & sessions) }
  , Disconnected
  )

userNotPartOfServer :: State -> (State, Response)
userNotPartOfServer state = (state, NotPartOfServer)
