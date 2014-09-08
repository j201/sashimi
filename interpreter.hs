import Parser
import Text.Parsec.Error
import Data.HashMap.Lazy hiding (map, filter)

type Scope = HashMap String SaVal

data SaVal = SaVal Literal | Closure Literal Scope | SaList [SaVal] deriving (Eq)

commaJoin = foldl1 (\s x -> s ++ ", " ++ x)

instance Show SaVal where
    show (SaVal v) = show v
    show (Closure v _) = show v
    show (SaList ss) = commaJoin (map show ss)

binOp :: (Double -> Double -> SaVal) -> Scope -> Expr -> Expr -> SaVal
binOp f s x y = case (evalExpr s x, evalExpr s y) of
                  (SaVal (Number x'), SaVal (Number y')) -> f x' y'

binOpN f = binOp (\x y -> SaVal $ Number (f x y))
binOpB f = binOp (\x y -> SaVal $ Boolean (f x y))

logicBinOp :: (Bool-> Bool -> Bool) -> Scope -> Expr -> Expr -> SaVal
logicBinOp f s x y = case (evalExpr s x, evalExpr s y) of
                       (SaVal (Boolean x'), SaVal (Boolean y')) -> SaVal (Boolean (f x' y'))

data Arity = NoRest Int Int | HasRest Int

matchArity :: Int -> Arity -> Bool
matchArity n (NoRest min max) = n >= min && n <= max
matchArity n (HasRest min) = n >= min

arity :: FunctionBody -> Arity
arity (FunctionBody bs _) = let min = (length $ filter (\(FunctionBinding _ d b) -> not b && d == Nothing) bs)
                            in if (any (\(FunctionBinding _ _ b) -> b) bs)
                               then HasRest min
                               else NoRest min (length bs)

evalExpr :: Scope -> Expr -> SaVal
evalExpr s (Literal (Function bs)) = Closure (Function bs) s
evalExpr _ (Literal l) = SaVal l
evalExpr s (Identifier i) = s ! i
evalExpr s (IfExpr a b c) = if case evalExpr s a of
                                 SaVal Nil -> False
                                 SaVal (Boolean False) -> False
                                 _ -> True
                            then evalExpr s c
                            else evalExpr s b
evalExpr s (LetExpr bs e) = evalExpr (foldl (\acc (i, v) -> insert i (evalExpr acc v) acc) s bs) e
evalExpr s (BinaryOp "+" x y) = binOpN (+) s x y
evalExpr s (BinaryOp "-" x y) = binOpN (-) s x y
evalExpr s (BinaryOp "*" x y) = binOpN (*) s x y
evalExpr s (BinaryOp "**" x y) = binOpN (**) s x y
evalExpr s (BinaryOp "/" x y) = binOpN (/) s x y
evalExpr s (BinaryOp ">" x y) = binOpB (>) s x y
evalExpr s (BinaryOp ">=" x y) = binOpB (>=) s x y
evalExpr s (BinaryOp "<" x y) = binOpB (<) s x y
evalExpr s (BinaryOp "<=" x y) = binOpB (<=) s x y
evalExpr s (BinaryOp "==" x y) = SaVal (Boolean (evalExpr s x == evalExpr s y))
evalExpr s (BinaryOp "!=" x y) = SaVal (Boolean (evalExpr s x /= evalExpr s y))
evalExpr s (BinaryOp "&" x y) = logicBinOp (&&) s x y
evalExpr s (BinaryOp "|" x y) = logicBinOp (||) s x y
evalExpr s (ExprGroup es) = last $ map (evalExpr s) es
evalExpr s (FunctionCall f args) = let Closure (Function bodies) fScope = evalExpr s f
                                       ba = head $ filter ((matchArity $ length args) . snd) $ zip bodies $ map arity bodies
                                       ar = snd ba
                                       (FunctionBody bs expr) = fst ba
                                       resolveBindings bs args = let notRest = (filter (\(FunctionBinding _ _ b) -> not b) bs)
                                                                     passedArgs = foldl (\acc (FunctionBinding name _ _, v) ->
                                                                                           insert name (evalExpr s v) acc)
                                                                                        (empty :: Scope) (zip notRest args)
                                                                     unused = drop (min (length notRest) (length args)) bs
                                                                  in foldl (\acc (FunctionBinding name def rest) ->
                                                                              if rest
                                                                              then insert name (SaList $ map (evalExpr s) $ drop (length notRest) args) acc
                                                                              else case def of { Just val -> insert name (evalExpr fScope val) acc })
                                                                           (passedArgs :: Scope) unused
                                   in evalExpr (union (resolveBindings bs args) fScope) expr

eval :: String -> Either ParseError SaVal
eval s = fmap (evalExpr empty) (parseExpr s)
