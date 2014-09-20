module Interpreter (evalFn, evalExpr, evalStatement, ProgState(..), toScope) where

import Parser
import InterpreterTypes
import Text.Parsec.Error
import Data.Hashable
import Data.HashMap.Lazy hiding (map, filter, foldr)
import qualified Data.HashMap.Strict as Strict
import Utils

-- todo: modules
-- loaded modules, current module, current scope
data ProgState = ProgState (HashMap String SaVal) (String, SaVal) Scope

unTag :: SaVal -> SaVal
unTag (TaggedVal v _) = v
unTag v = v

toNumber :: SaVal -> Double
toNumber (Primitive (Number x)) = x
toNumber (TaggedVal (Primitive (Number x)) _) = x

toBoolean :: SaVal -> Bool
toBoolean (Primitive (Boolean b)) = b
toBoolean (TaggedVal (Primitive (Boolean b)) _) = b

binOp :: (SaVal -> a) -> (a -> a -> SaVal) -> Scope -> Expr -> Expr -> SaVal
binOp conv f s x y = f (conv (evalExpr s x)) (conv (evalExpr s y))

binOpNN f = binOp toNumber (\x y -> Primitive $ Number (f x y))
binOpNB f = binOp toNumber (\x y -> Primitive $ Boolean (f x y))
binOpBB f = binOp toBoolean (\x y -> Primitive $ Boolean $ f x y)

data Arity = NoRest Int Int | HasRest Int

matchArity :: Int -> Arity -> Bool
matchArity n (NoRest min max) = n >= min && n <= max
matchArity n (HasRest min) = n >= min

arity :: FunctionBody -> Arity
arity (FunctionBody bs _) = let min = (length $ filter (\(FunctionBinding _ d b) -> not b && d == Nothing) bs)
                            in if (any (\(FunctionBinding _ _ b) -> b) bs)
                               then HasRest min
                               else NoRest min (length bs)

-- expects a Keyword->SaVal SaMap
toScope :: SaVal -> Scope
toScope (SaMap hm) = mapKeys (\(Primitive (Keyword k)) -> k) hm

evalClosure :: SaVal -> [SaVal] -> SaVal
evalClosure (Closure (Function bodies) fScope) args =
    let ba = head $ filter ((matchArity $ length args) . snd) $ zip bodies $ map arity bodies
        fArity = snd ba
        (FunctionBody bindings expr) = fst ba
        resolveBindings bindings args = let notRest = (filter (\(FunctionBinding _ _ b) -> not b) bindings)
                                            passedArgs = foldl (\acc (FunctionBinding name _ _, v) ->
                                                                insert name v acc)
                                                                empty (zip notRest args)
                                            unused = drop (min (length notRest) (length args)) bindings
                                        in foldl (\acc (FunctionBinding name def rest) ->
                                                    if rest
                                                    then insert name (toSaList $ drop (length notRest) args) acc
                                                    else case def of { Just val -> insert name (evalExpr fScope val) acc })
                                                 passedArgs unused
    in evalExpr (union (resolveBindings bindings args) fScope) expr

evalFn :: SaVal -> [SaVal] -> SaVal
evalFn (NativeFunction f) = f
evalFn (TagFn fns mdef) = (\args -> case (args, mdef) of
                                      ([], Just f) -> evalFn f []
                                      (((TaggedVal v tags):xs), _) -> case maybeHead $ filter (/= Nothing) $ map ((flip Strict.lookup) fns) tags of
                                                                        (Just (Just f)) -> evalFn f (v:xs)
                                                                        Nothing -> case mdef of { (Just f) -> evalFn f (v:xs) }
                                      (xs, Just f) -> evalFn f xs)

evalFn x = evalClosure x

evalExpr :: Scope -> Expr -> SaVal
evalExpr s (Literal (Function bs)) = Closure (Function bs) s
evalExpr s (Literal (List xs)) = toSaList (map (evalExpr s) xs)
evalExpr _ (Literal l) = Primitive l
evalExpr s (Identifier i) = s ! i
evalExpr s (IfExpr a b c) = if case unTag (evalExpr s a) of
                                 Primitive Nil -> False
                                 Primitive (Boolean False) -> False
                                 _ -> True
                            then evalExpr s b
                            else evalExpr s c
evalExpr s (LetExpr bs e) = evalExpr (foldl (\acc (i, v) ->
                                               insert i (case evalExpr acc v of
                                                          (Closure l s) -> let c = Closure l (insert i c s) in c
                                                          x -> x)
                                                        acc) s bs) e
evalExpr s (BinaryOp "+" x y) = binOpNN (+) s x y
evalExpr s (BinaryOp "-" x y) = binOpNN (-) s x y
evalExpr s (BinaryOp "*" x y) = binOpNN (*) s x y
evalExpr s (BinaryOp "**" x y) = binOpNN (**) s x y
evalExpr s (BinaryOp "/" x y) = binOpNN (/) s x y
evalExpr s (BinaryOp ">" x y) = binOpNB (>) s x y
evalExpr s (BinaryOp ">=" x y) = binOpNB (>=) s x y
evalExpr s (BinaryOp "<" x y) = binOpNB (<) s x y
evalExpr s (BinaryOp "<=" x y) = binOpNB (<=) s x y
evalExpr s (BinaryOp "==" x y) = Primitive (Boolean (evalExpr s x == evalExpr s y)) -- TODO: deal w
evalExpr s (BinaryOp "!=" x y) = Primitive (Boolean (evalExpr s x /= evalExpr s y))
evalExpr s (BinaryOp "&" x y) = binOpBB (&&) s x y
evalExpr s (BinaryOp "|" x y) = binOpBB (||) s x y
evalExpr s (BinaryOp "~" x y) = case (evalExpr s x, evalExpr s y) of
                                  (TaggedVal v tags, Primitive (Keyword tag)) -> TaggedVal v (tag:tags)
                                  (v, Primitive (Keyword tag)) -> TaggedVal v [tag]
evalExpr s (UnaryOp "!" x) = case unTag $ evalExpr s x of
                               (Primitive (Boolean b)) -> (Primitive $ Boolean $ not b)
evalExpr s (UnaryOp "-" x) = case unTag $ evalExpr s x of
                               (Primitive (Number x)) -> (Primitive $ Number (-x))
evalExpr s (ExprGroup es) = last $ map (evalExpr s) es
evalExpr s (MapAccess m kw) = case unTag $ evalExpr s m of
                                (SaMap m) -> m ! (Primitive $ Keyword kw)
evalExpr s (FunctionCall f args) = evalFn (evalExpr s f) (map (evalExpr s) args)

insKW :: String -> SaVal -> SaVal -> SaVal
insKW k v (SaMap m) = SaMap (Strict.insert (Primitive $ Keyword k) v m)

memberSaMap :: String -> SaVal -> Bool
memberSaMap k (SaMap m) = member (Primitive $ Keyword k) m

-- default state (for new modules), current program state, statement, new program state
evalStatement :: Scope -> ProgState -> Statement -> ProgState
evalStatement _ (ProgState ms m s) (Definition name expr) = ProgState ms m (insert name (evalExpr s expr) s) -- TODO: deal with TagFn
evalStatement defaultScope (ProgState ms (mName, mVal) s) (ModuleDeclaration newMName) = if mName /= ""
                                                                                         then ProgState (insert mName mVal ms) (newMName, SaMap empty) defaultScope
                                                                                         else ProgState ms (newMName, SaMap empty) defaultScope
evalStatement _ (ProgState ms (mName, mMap) s) (ExportedDefinition name expr) = let val = case evalExpr s expr of
                                                                                          (Closure l s') -> let c = Closure l (insert name c s') in c
                                                                                          x -> x
                                                                                in ProgState ms (mName, insKW name val mMap) (insert name val s)
evalStatement _ (ProgState ms (mName, mMap) s) (MethodDefinition tag fname expr) = let newTagFn = case Strict.lookup fname s of
                                                                                                    (Just (TagFn hm def)) -> TagFn (insert tag (evalExpr s expr) hm) def
                                                                                                    (Just nf@(NativeFunction _)) -> TagFn (insert tag (evalExpr s expr) empty) (Just nf)
                                                                                                    (Just c@(Closure _ _)) -> TagFn (insert tag (evalExpr s expr) empty) (Just c)
                                                                                                    Nothing -> TagFn (insert tag (evalExpr s expr) empty) Nothing
                                                                                   in ProgState ms
                                                                                                (if memberSaMap fname mMap
                                                                                                 then (mName, insKW fname newTagFn mMap)
                                                                                                 else (mName, mMap))
                                                                                                (insert fname newTagFn s)
