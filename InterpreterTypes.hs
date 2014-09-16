module InterpreterTypes (Scope, SaVal(..)) where

type Scope = HashMap String SaVal

data SaVal = Primitive Literal
           | Closure Literal Scope
           | SaList [SaVal]
           | SaMap (Strict.HashMap SaVal SaVal)
           | NativeFunction ([SaVal] -> SaVal)

instance Eq SaVal where
    (NativeFunction _) == _ = False
    (Closure _ _) == _ = False
    (Primitive l1) == (Primitive l2) = l1 == l2
    (SaList l1) == (SaList l2) = l1 == l2
    _ == _ = False

instance Show SaVal where
    show (Primitive v) = show v
    show (Closure v _) = show v
    show (SaList ss) = commaJoin (map show ss)
    show (NativeFunction _) = "(native)"

-- TODO: complete
instance Hashable SaVal where
    hashWithSalt n (Primitive (String s)) = hashWithSalt n s
    hashWithSalt n (Primitive (Keyword s)) = hashWithSalt n ('.' : s)

