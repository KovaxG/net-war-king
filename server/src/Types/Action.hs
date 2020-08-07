module Types.Action where

import qualified Data.Char as Char

import Types.Common

data Action
  = Login PlayerName Password
  | Logout
  | SetReady Bool
  deriving (Read, Show)

actionParser :: String -> Maybe Action
actionParser s = case fmap (fmap Char.toLower) $ words s of
  ["lo"] -> Just Logout
  ['l':_, player, password] -> Just $ Login player password
  ["sr", "false"] -> Just $ SetReady False
  ["sr", "true"] -> Just $ SetReady True
  _ -> Nothing
