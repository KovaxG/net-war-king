module Types.Response where

import Common
import Constants
import Types.Common
import Types.State

data Response
  = LobbyJoinSuccess
  | LobbyJoinFailDuplicatedName
  | NotImplemented
  | Test String
  | LoggedInFromDifferentSession
  | SessionAlreadyLoggedIn
  | Illegal
  | LoginSuccess
  | BadPassword
  | Disconnected
  | NotPartOfServer
  deriving (Show, Eq)
