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
        SetReady ready -> (state, NotImplemented)

    Game ->
      case action of
        Logout -> removeSession sessionID state
        Login user pass ->
          loginFlow sessionID user pass state (userNotPartOfServer state)
        SetReady _ -> illegal state

loginFlow :: SessionID -> PlayerName -> Password -> State -> (State, Response) -> (State, Response)
loginFlow sessionID user pass state newUserFlow =
  if Just sessionID ∈ Map.map session (state & players)
  then sessionAlreadyInUse state
  else
    case (state & players) !? user of
      Nothing -> newUserFlow
      Just playerData ->
        if Maybe.isJust (playerData & session)
        then userAlreadyLoggedIn state
        else
          if (playerData & password) == pass
          then addSession user sessionID state
          else badPassword state

badPassword :: State -> (State, Response)
badPassword state = (state, BadPassword)

addSession :: PlayerName -> SessionID -> State -> (State, Response)
addSession user sessionID state =
  ( state { players = Map.adjust (\pd -> pd { session = Just sessionID }) user (state & players) }
  , LoginSuccess
  )

joinServerAndAddSession :: PlayerName -> Password -> SessionID -> State -> (State, Response)
joinServerAndAddSession user pass sessionID state =
  ( state { players = Map.insert user playerData (state & players) }
  , LobbyJoinSuccess
  )
  where
    playerData = PlayerData
      { password = pass
      , ready = False
      , session = Just sessionID
      }

userAlreadyLoggedIn :: State -> (State, Response)
userAlreadyLoggedIn state = (state, LoggedInFromDifferentSession)

sessionAlreadyInUse :: State -> (State, Response)
sessionAlreadyInUse state = (state, SessionAlreadyLoggedIn)

removeSession :: SessionID -> State -> (State, Response)
removeSession sessionID state =
  ( state {
      players =
        Map.map (\pd ->
          if (pd & session) == Just sessionID
          then pd { session = Nothing }
          else pd
        ) (state & players)
    }
  , Disconnected
  )

userNotPartOfServer :: State -> (State, Response)
userNotPartOfServer state = (state, NotPartOfServer)

illegal :: State -> (State, Response)
illegal state = (state, Illegal)
