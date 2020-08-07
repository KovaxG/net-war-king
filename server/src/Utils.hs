module Utils where

import qualified Data.Maybe as Maybe

safeRead :: Read a => String -> Maybe a
safeRead = fmap fst . Maybe.listToMaybe . reads

mapIf :: Functor f => (a -> Bool) -> (a -> a) -> f a -> f a
mapIf p f = fmap (\a -> if p a then f a else a)
