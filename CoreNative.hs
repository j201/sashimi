module CoreNative (SaVal(..), Scope(..), nativeFns) where

import Parser
import InterpreterTypes
import Interpreter
import Data.HashMap.Lazy hiding (map, filter, foldr)
import qualified Data.HashMap.Strict as Strict

commaJoin = foldl1 (\s x -> s ++ ", " ++ x)

nativeFns :: SaVal
nativeFns = SaMap $ Strict.fromList $
            map (\(a,b) -> (Primitive $ Keyword a, NativeFunction b)) $
            [
             ("first", \[(SaList xs)] -> head xs),

             ("rest", \[(SaList xs)] -> SaList $ tail xs),

             ("empty", \[(SaList xs)] -> Primitive $ Boolean $ xs == []),

             ("cons", \[x, (SaList xs)] -> SaList (x:xs)),

             ("reduce", \args -> case args of
                                   [(SaList xs), i, f] -> foldl (\a b -> (evalFn f) [b, a]) i xs -- Not sure if this will reevaluate (evalFn f)
                                   [(SaList (x:xs)), f] -> foldl (\a b -> (evalFn f) [b, a]) x xs
                                   [(SaList []), f] -> (evalFn f) []),
                                   
             ("reduceRight", \args -> case args of
                                   [(SaList xs), i, f] -> foldr (\a b -> (evalFn f) [b, a]) i xs
                                   [(SaList (x:xs)), f] -> foldr (\a b -> (evalFn f) [b, a]) x xs
                                   [(SaList []), f] -> (evalFn f) []),
                                   
             ("range", \args -> case args of
                                  [(Primitive (Number x)), (Primitive (Number y)), (Primitive (Number z))] -> SaList $ map (Primitive . Number) [x,(x+z)..(y-1)] -- TODO: haskell does silly overshoot stuff (see [0,2..9.0])
                                  [(Primitive (Number x)), (Primitive (Number y))] -> SaList $ map (Primitive . Number) [x..(y-1)]
                                  [(Primitive (Number x))] -> SaList $ map (Primitive . Number) [x..]
                                  [] -> SaList $ map (Primitive . Number) [0..])
            ]
