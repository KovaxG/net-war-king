module Utils where

import qualified Data.Maybe as Maybe

safeRead :: Read a => String -> Maybe a
safeRead = fmap fst . Maybe.listToMaybe . reads