module UpdateSpec (spec) where

import           Data.Foldable
import           Data.Map (Map)
import qualified Data.Map as Map
import           Test.Hspec (Spec)
import qualified Test.Hspec as Test

import Common
import Constants
import Logic.Update
import TestUtils
import Types.Action
import Types.Response
import Types.State

spec :: Spec
spec = do
  Test.describe "update function" $ do
    Test.it "a new player joining the lobby" $ do
      let name = "Kovax"
      let pass = "pass"
      let action = Login name pass

      let session = "session_1"
      let state = State
            { mode = Lobby
            , players = Map.empty
            }
      let (newState, response) = update session action state

      sequence_
        [ newState === State
            { mode = Lobby
            , players =
                Map.fromList
                  [ ( name
                    , PlayerData
                      { password = pass
                      , ready = False
                      , session = Just session
                      }
                    )
                  ]
            }
        , response === LobbyJoinSuccess
        ]

    Test.it "a player logging out of the lobby" $ do
      let action = Logout

      let name = "Kovax"
      let pass = "pass"
      let session = "session_1"
      let state = State
            { mode = Lobby
            , players = Map.fromList
                [ ( name
                  , PlayerData
                    { password = pass
                    , ready = False
                    , session = Just session
                    }
                  )
                ]
            }
      let (newState, response) = update session action state

      sequence_
        [ newState === State
            { mode = Lobby
            , players =
                Map.fromList
                  [ ( name
                    , PlayerData
                      { password = pass
                      , ready = False
                      , session = Nothing
                      }
                    )
                  ]
            }
        , response === Disconnected
        ]

    Test.it "trying to log into a player logged in on another session" $ do
      let name = "Kovax"
      let pass = "pass"
      let action = Login name pass

      let session1 = "session_1"
      let session2 = "session_2"

      let state = State
              { mode = Lobby
              , players = Map.fromList
                  [ ( name
                    , PlayerData
                      { password = pass
                      , ready = False
                      , session = Just session1
                      }
                    )
                  ]
              }
      let (newState, response) = update session2 action state

      sequence_
        [ newState === State
            { mode = Lobby
            , players =
                Map.fromList
                  [ ( name
                    , PlayerData
                      { password = pass
                      , ready = False
                      , session = Just session1
                      }
                    )
                  ]
            }
        , response === LoggedInFromDifferentSession
        ]

    Test.it "trying to log into two accounts from a single session" $ do
      let name1 = "Kovax"
      let pass1 = "pass"

      let name2 = "Guri"
      let pass2 = "passwort"

      let action = Login name2 pass2

      let session = "session_1"

      let state = State
              { mode = Lobby
              , players = Map.fromList
                  [ ( name1
                    , PlayerData
                      { password = pass1
                      , ready = False
                      , session = Just session
                      }
                    ),
                    ( name2
                    , PlayerData
                      { password = pass2
                      , ready = False
                      , session = Nothing
                      }
                    )
                  ]
              }
      let (newState, response) = update session action state

      sequence_
        [ newState === State
            { mode = Lobby
              , players = Map.fromList
                  [ ( name1
                    , PlayerData
                      { password = pass1
                      , ready = False
                      , session = Just session
                      }
                    ),
                    ( name2
                    , PlayerData
                      { password = pass2
                      , ready = False
                      , session = Nothing
                      }
                    )
                  ]
              }
        , response === SessionAlreadyLoggedIn
        ]

    Test.it "trying to log into two accounts from a single session, while the other already has a running session" $ do
      let name1 = "Kovax"
      let pass1 = "pass"

      let name2 = "Guri"
      let pass2 = "passwort"

      let action = Login name2 pass2

      let session1 = "session_1"
      let session2 = "session_2"

      let state = State
              { mode = Lobby
              , players = Map.fromList
                  [ ( name1
                    , PlayerData
                      { password = pass1
                      , ready = False
                      , session = Just session1
                      }
                    ),
                    ( name2
                    , PlayerData
                      { password = pass2
                      , ready = False
                      , session = Just session2
                      }
                    )
                  ]
              }
      let (newState, response) = update session1 action state

      sequence_
        [ newState === State
            { mode = Lobby
              , players = Map.fromList
                  [ ( name1
                    , PlayerData
                      { password = pass1
                      , ready = False
                      , session = Just session1
                      }
                    ),
                    ( name2
                    , PlayerData
                      { password = pass2
                      , ready = False
                      , session = Just session2
                      }
                    )
                  ]
              }
        , response === SessionAlreadyLoggedIn
        ]

    Test.it "setting readyness" $ do
      let name1 = "Kovax"
      let pass1 = "pass"

      let name2 = "Guri"
      let pass2 = "passwort"

      let action = SetReady True

      let session1 = "session_1"
      let session2 = "session_2"

      let state = State
              { mode = Lobby
              , players = Map.fromList
                  [ ( name1
                    , PlayerData
                      { password = pass1
                      , ready = False
                      , session = Just session1
                      }
                    ),
                    ( name2
                    , PlayerData
                      { password = pass2
                      , ready = False
                      , session = Just session2
                      }
                    )
                  ]
              }
      let (newState, response) = update session1 action state

      sequence_
        [ newState === State
            { mode = Lobby
              , players = Map.fromList
                  [ ( name1
                    , PlayerData
                      { password = pass1
                      , ready = True
                      , session = Just session1
                      }
                    ),
                    ( name2
                    , PlayerData
                      { password = pass2
                      , ready = False
                      , session = Just session2
                      }
                    )
                  ]
              }
        , response === Ok
        ]
