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

defaultScope :: Scope
defaultScope = let (Right ss) = parseSashimi coreText
                   (ProgState _ (_, (SaMap m)) _) = foldl (evalStatement nativeFnsScope) emptyState ss
               in mapKeys (\(Primitive (Keyword s)) -> s) m -- core should only use keyword exports

eval :: String -> Either ParseError SaVal
eval s = fmap (evalExpr defaultScope) (parseExpr s)
