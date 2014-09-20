module CoreNative (SaVal(..), Scope(..), nativeFns) where

import Parser
import InterpreterTypes
import Interpreter
import Data.HashMap.Lazy hiding (map, filter, foldr)
import qualified Data.HashMap.Strict as Strict

commaJoin = foldl1 (\s x -> s ++ ", " ++ x)

saFirst (SaList x _) = x
saFirst (LazyList ll) = case ll() of
                          LazyListMore t -> fst $ t
                          LazyListStrict s -> saFirst s

saRest (Primitive Nil) = Primitive Nil
saRest (SaList _ xs) = xs
saRest (LazyList ll) = case ll() of
                         LazyListMore t -> (snd $ t) ()
                         LazyListStrict s -> saRest s

nativeFns :: SaVal
nativeFns = SaMap $ Strict.fromList $
            map (\(a,b) -> (Primitive $ Keyword a, NativeFunction b)) $
            [
             ("first", \[arg] -> saFirst arg),

             ("rest", \[arg] -> saRest arg),

             ("cons", \[x, xs] -> SaList x xs),

             ("lazyCons", \[x, f] -> LazyListRet $ LazyListMore (x, \_ -> (evalFn f) [])),

             ("lazyList", \[f] -> LazyList (\_ -> case evalFn f [] of
                                                    LazyListRet r -> r
                                                    x -> LazyListStrict x))
            ]
