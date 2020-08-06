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

update :: Action -> State -> State
update a (State n) = State $ n+1
