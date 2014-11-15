import Parser
import InterpreterTypes
import CoreNative
import Interpreter
import Utils
import SashimiCore
import Text.Parsec.Error
import Data.HashMap.Lazy hiding (map, filter)

nativeFnsScope :: Scope
nativeFnsScope = toScope nativeFns

nativeFnsState :: ProgState
nativeFnsState = ProgState empty ("", SaMap empty) defaultScope

emptyState :: ProgState
emptyState = ProgState empty ("", SaMap empty) empty

defaultScope :: Scope
defaultScope = union nativeFnsScope $
               let (Right ss) = parseSashimi coreText
                   (ProgState _ (_, (SaMap m)) _) = foldl (evalStatement nativeFnsScope) nativeFnsState ss
               in mapKeys (\(Primitive (Keyword s)) -> s) m -- core should only use keyword exports

defaultState :: ProgState
defaultState = ProgState empty ("", SaMap empty) defaultScope

eval :: String -> Either ParseError SaVal
eval s = fmap (evalExpr defaultScope) (parseExpr s)

-- evals statements followed by an expression
evals :: String -> Either ParseError SaVal
evals s = fmap (\ss ->
                  let (ProgState _ _ scope) = foldl (evalStatement defaultScope) defaultState $ init ss
                  in case last ss of
                      (Expression e) -> evalExpr scope e
                      _ -> error "Last statement must be an expression")
               (parseSashimi s)
