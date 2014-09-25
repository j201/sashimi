module InterpreterTypes (Scope, SaVal(..), toSaList, LazyListRet(..), defaultTag, toSaMap) where

import Parser
import Data.Hashable
import Data.HashMap.Lazy hiding (map, filter, foldr)
import qualified Data.HashMap.Strict as Strict
import Utils

type Scope = HashMap String SaVal

data LazyListRet = LazyListMore (SaVal, () -> SaVal)
                 | LazyListStrict SaVal

strictList :: SaVal -> SaVal
strictList (LazyList ll) = strictList $ LazyListRet $ ll()
strictList (LazyListRet (LazyListMore (v, f))) = SaList v (strictList $ f())
strictList (LazyListRet (LazyListStrict x)) = x
strictList x = x

instance Eq LazyListRet where
    LazyListStrict s1 == LazyListStrict s2 = s1 == s2
    LazyListMore (x1, xs1) == LazyListMore (x2, xs2) = x1 == x2 && xs1() == xs2()
    _ == _ = False

instance Show LazyListRet where
    show (LazyListMore (x, xs)) = show x ++ ":" ++ show (xs())
    show (LazyListStrict s) = show s

instance Hashable LazyListRet where
    hashWithSalt n llr = hashWithSalt n (strictList $ LazyListRet llr)

data SaVal = Primitive Literal
           | Closure Literal Scope
           | SaList SaVal SaVal
           | LazyListRet LazyListRet
           | LazyList (() -> LazyListRet) -- the function should return a list where the first element is the first LazyList element, and the second is a function that returns the rest
           | SaMap (Strict.HashMap SaVal SaVal)
           | NativeFunction ([SaVal] -> SaVal)
           | TaggedVal SaVal [String]
           | TagFunction (HashMap String SaVal) (Maybe SaVal) -- second value is default

instance Eq SaVal where
    (Primitive l1) == (Primitive l2) = l1 == l2
    (SaList x1 xs1) == (SaList x2 xs2) = x1 == x2 && xs1 == xs2
    (LazyList ll1) == (LazyList ll2) = ll1() == ll2()
    (SaMap m1) == (SaMap m2) = m1 == m2
    -- TODO: include default tags, should all tags match? (i think so)
    -- (TaggedVal v1 tags1) == (TaggedVal v2 tags2) = v1 == v2 && head tags1 == head tags2
    -- (TaggedVal v1 _) == v2 = v1 == v2
    _ == _ = False

instance Show SaVal where
    show (Primitive v) = show v
    show (Closure v _) = show v
    show (SaList x xs) = show x ++ ":" ++ show xs
    show (LazyList ll) = show $ ll()
    show (NativeFunction _) = "(native)"

-- TODO: complete
instance Hashable SaVal where
    hashWithSalt n (Primitive (String s)) = hashWithSalt n ('s' : s)
    hashWithSalt n (Primitive (Keyword s)) = hashWithSalt n ('k' : s)
    hashWithSalt n (Primitive (Number x)) = hashWithSalt n ('n' : show x)
    hashWithSalt n (Primitive (Regex s)) = hashWithSalt n ('r' : s)
    hashWithSalt n (Primitive Nil) = hashWithSalt n "Nil"
    hashWithSalt n (Primitive (Boolean b)) = hashWithSalt n ('b' : show b)
    hashWithSalt n (SaList x xs) = hashWithSalt n x + hashWithSalt n xs
    hashWithSalt n (SaMap xs) = Strict.foldl' (flip $ (+) . (hashWithSalt n)) 0 xs
    hashWithSalt n (LazyList ll) = hashWithSalt n $ ll()

defaultTag :: SaVal -> String
defaultTag (Primitive Nil) ="Nil"
defaultTag (Primitive (Number _)) ="Number"
defaultTag (Primitive (String _)) ="String"
defaultTag (Primitive (Boolean _)) ="Boolean"
defaultTag (Primitive (Regex _)) ="Regex"
defaultTag (Primitive (Keyword _)) ="Keyword"
defaultTag (Closure _ _) = "Function"
defaultTag (NativeFunction _) = "Function"
defaultTag (TagFunction _ _) = "Function"
defaultTag (SaList _ _) = "List"
defaultTag (SaMap _) = "Map"

toSaList :: [SaVal] -> SaVal
toSaList = foldr SaList (Primitive Nil)

toSaMap :: [SaVal] -> SaVal
toSaMap = let pairOff (x:y:xs) = (x, y) : (pairOff xs)
              pairOff [] = []
          in SaMap . Strict.fromList . pairOff
