module Utils (commaJoin, mapKeys) where

import Data.Hashable
import Data.HashMap.Lazy hiding (map, filter)

mapKeys :: (Hashable k2, Eq k2) => (k -> k2) -> HashMap k v -> HashMap k2 v
mapKeys f = foldlWithKey' (\res k v -> insert (f k) v res) empty

commaJoin = foldl1 (\s x -> s ++ ", " ++ x)
