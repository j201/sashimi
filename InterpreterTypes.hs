module InterpreterTypes (Scope, SaVal(..), toSaList, LazyListRet(..)) where

import Parser
import Data.Hashable
import Data.HashMap.Lazy hiding (map, filter, foldr)
import qualified Data.HashMap.Strict as Strict
import Utils

type Scope = HashMap String SaVal

data LazyListRet = LazyListMore (SaVal, () -> SaVal)
                 | LazyListStrict SaVal

instance Eq LazyListRet where
    LazyListStrict s1 == LazyListStrict s2 = s1 == s2
    LazyListMore (x1, xs1) == LazyListMore (x2, xs2) = x1 == x2 && xs1() == xs2()
    _ == _ = False

instance Show LazyListRet where
    show (LazyListMore (x, xs)) = show x ++ ":" ++ show (xs())
    show (LazyListStrict s) = show s

data SaVal = Primitive Literal
           | Closure Literal Scope
           | SaList SaVal SaVal
           | LazyListRet LazyListRet
           | LazyList (() -> LazyListRet) -- the function should return a list where the first element is the first LazyList element, and the second is a function that returns the rest
           | SaMap (Strict.HashMap SaVal SaVal)
           | NativeFunction ([SaVal] -> SaVal)
           | TaggedVal SaVal [String]
           | TagFn (HashMap String SaVal) (Maybe SaVal) -- second value is default

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
    hashWithSalt n (Primitive (String s)) = hashWithSalt n s
    hashWithSalt n (Primitive (Keyword s)) = hashWithSalt n ('.' : s)

toSaList :: [SaVal] -> SaVal
toSaList = foldr SaList (Primitive Nil)
