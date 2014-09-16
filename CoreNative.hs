module CoreNative (SaVal(..), Scope(..), nativeFns) where

import Parser
import InterpreterTypes
import Data.Hashable
import Data.HashMap.Lazy hiding (map, filter)
import qualified Data.HashMap.Strict as Strict

commaJoin = foldl1 (\s x -> s ++ ", " ++ x)

nativeFns :: SaVal
nativeFns = SaMap $ Strict.fromList $
            map (\(a,b) -> (Primitive $ Keyword a, NativeFunction b)) $
            [("first", \[(SaList xs)] -> head xs),
             ("rest", \[(SaList xs)] -> SaList $ tail xs),
             ("empty", \[(SaList xs)] -> Primitive $ Boolean $ xs == []),
             ("cons", \[x, (SaList xs)] -> SaList (x:xs))]
