module CoreNative (SaVal(..), Scope(..), nativeFns) where

import Parser
import Data.HashMap.Lazy hiding (map, filter)

type Scope = HashMap String SaVal

data SaVal = SaVal Literal | Closure Literal Scope | SaList [SaVal] deriving (Eq)

nativeFns :: HashMap String ([SaVal] -> SaVal)
nativeFns = fromList [("first", \[(SaList xs)] -> head xs),
                      ("rest", \[(SaList xs)] -> SaList $ tail xs)]
