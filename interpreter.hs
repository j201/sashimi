module Interpreter (evalFn, evalExpr, evalStatement, ProgState(..), toScope) where

import Parser
import InterpreterTypes
import Text.Parsec.Error
import Data.Hashable
import Data.HashMap.Lazy hiding (map, filter)
import qualified Data.HashMap.Strict as Strict
import Utils

-- todo: modules
-- loaded modules, current module, current scope
data ProgState = ProgState (HashMap String SaVal) (String, SaVal) Scope

binOp :: (Double -> Double -> SaVal) -> Scope -> Expr -> Expr -> SaVal
binOp f s x y = case (evalExpr s x, evalExpr s y) of
                  (Primitive (Number x'), Primitive (Number y')) -> f x' y'

binOpN f = binOp (\x y -> Primitive $ Number (f x y))
binOpB f = binOp (\x y -> Primitive $ Boolean (f x y))

logicBinOp :: (Bool-> Bool -> Bool) -> Scope -> Expr -> Expr -> SaVal
logicBinOp f s x y = case (evalExpr s x, evalExpr s y) of
                       (Primitive (Boolean x'), Primitive (Boolean y')) -> Primitive (Boolean (f x' y'))

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
                                                    then insert name (SaList $ drop (length notRest) args) acc
                                                    else case def of { Just val -> insert name (evalExpr fScope val) acc })
                                                 passedArgs unused
    in evalExpr (union (resolveBindings bindings args) fScope) expr

evalFn :: SaVal -> [SaVal] -> SaVal
evalFn (NativeFunction f) = f
evalFn (TagFn fns mdef) = let this = (\args -> case (args, mdef) of
                                                 ([], Just f) -> evalFn f []
                                                 (((TaggedVal v (t:ts)):xs), _) -> case lookup t fns of
evalFn x = evalClosure x

evalExpr :: Scope -> Expr -> SaVal
evalExpr s (Literal (Function bs)) = Closure (Function bs) s
evalExpr s (Literal (List xs)) = SaList (map (evalExpr s) xs)
evalExpr _ (Literal l) = Primitive l
evalExpr s (Identifier i) = s ! i
evalExpr s (IfExpr a b c) = if case evalExpr s a of
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
evalExpr s (BinaryOp "+" x y) = binOpN (+) s x y
evalExpr s (BinaryOp "-" x y) = binOpN (-) s x y
evalExpr s (BinaryOp "*" x y) = binOpN (*) s x y
evalExpr s (BinaryOp "**" x y) = binOpN (**) s x y
evalExpr s (BinaryOp "/" x y) = binOpN (/) s x y
evalExpr s (BinaryOp ">" x y) = binOpB (>) s x y
evalExpr s (BinaryOp ">=" x y) = binOpB (>=) s x y
evalExpr s (BinaryOp "<" x y) = binOpB (<) s x y
evalExpr s (BinaryOp "<=" x y) = binOpB (<=) s x y
evalExpr s (BinaryOp "==" x y) = Primitive (Boolean (evalExpr s x == evalExpr s y))
evalExpr s (BinaryOp "!=" x y) = Primitive (Boolean (evalExpr s x /= evalExpr s y))
evalExpr s (BinaryOp "&" x y) = logicBinOp (&&) s x y
evalExpr s (BinaryOp "|" x y) = logicBinOp (||) s x y
evalExpr s (BinaryOp "~" x y) = case evalExpr s y of -- TODO: switch over to [String]
                                  (Primitive (String tag)) -> TaggedVal (evalExpr s x) tag
evalExpr s (UnaryOp "!" x) = case evalExpr s x of
                               (Primitive (Boolean b)) -> (Primitive $ Boolean $ not b)
evalExpr s (UnaryOp "-" x) = case evalExpr s x of
                               (Primitive (Number x)) -> (Primitive $ Number (-x))
evalExpr s (ExprGroup es) = last $ map (evalExpr s) es
evalExpr s (MapAccess m kw) = case evalExpr s m of
                                (SaMap m) -> m ! (Primitive $ Keyword kw)
evalExpr s (FunctionCall f args) = evalFn (evalExpr s f) (map (evalExpr s) args)

insKW :: String -> SaVal -> SaVal -> SaVal
insKW k v (SaMap m) = SaMap (Strict.insert (Primitive $ Keyword k) v m)

-- default state (for new modules), current program state, statement, new program state
evalStatement :: Scope -> ProgState -> Statement -> ProgState
evalStatement _ (ProgState ms m s) (Definition name expr) = ProgState ms m (insert name (evalExpr s expr) s)
evalStatement defaultScope (ProgState ms (mName, mVal) s) (ModuleDeclaration newMName) = if mName /= ""
                                                                                         then ProgState (insert mName mVal ms) (newMName, SaMap empty) defaultScope -- todo: use defaultScope
                                                                                         else ProgState ms (newMName, SaMap empty) defaultScope
evalStatement _ (ProgState ms (mName, mMap) s) (ExportedDefinition name expr) = let val = case evalExpr s expr of
                                                                                          (Closure l s') -> let c = Closure l (insert name c s') in c
                                                                                          x -> x
                                                                              in ProgState ms (mName, insKW name val mMap) (insert name val s)
