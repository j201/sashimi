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

saRest EmptyList = EmptyList
saRest (SaList _ xs) = xs
saRest (LazyList ll) = case ll() of
                         LazyListMore t -> (snd $ t) ()
                         LazyListStrict s -> saRest s

saListGet :: Int -> SaVal -> SaVal
saListGet n (SaList x xs) = if n < 1
                            then x
                            else saListGet (n-1) xs

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
                                                    x -> LazyListStrict x)),

             ("get", \args -> case args of
                                [(SaMap m), k] -> case Strict.lookup k m of
                                                   (Just x) -> x
                                                   Nothing -> Primitive Nil
                                [xs@(SaList _ _), (Primitive (Number n))] -> saListGet (floor n) xs)
            ]
